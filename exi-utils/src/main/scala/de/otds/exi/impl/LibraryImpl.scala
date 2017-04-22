package de.otds.exi.impl

import java.io.{File, _}
import java.util.Properties
import javax.xml.XMLConstants
import javax.xml.transform.Source
import javax.xml.validation.SchemaFactory

import de.otds.exi._

import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

trait LibraryImpl extends ResourceManager {
  val library: Library

  protected def io[R](inputFile: File, outputFile: File)(operation: (BufferedInputStream, BufferedOutputStream) => R): Try[R] = {
    withResource(new BufferedInputStream(new FileInputStream(inputFile))) {
      is =>
        withResource(new BufferedOutputStream(new FileOutputStream(outputFile))) {
          os =>
            Try(operation(is, os))
        }
    }
  }

  /**
    * Indicates whether the given combination of options are supported by this implementation.
    *
    * @return True if the combination of options is supported
    */
  def supports(codingMode: CodingMode, fidelityOptionMode: FidelityOptionMode): Boolean

  def encode(inputStream: BufferedInputStream, outputStream: BufferedOutputStream, settings: Settings): Unit

  def encode(inputFile: XmlFile, outputDir: Directory, settings: Settings): EncodeResult = {
    outputDir.mkdirs()
    val outputFile = new ExiFile(outputDir, inputFile.getName + s".${library.id.toLowerCase}.${settings.codingMode.id.toLowerCase}.${settings.fidelityOptionMode.id.toLowerCase()}.exi")

    val result = io(inputFile, outputFile)(encode(_, _, settings)) match {
      case Success(_) =>
        EncodeResult(inputFile, outputFile, settings, ok = true, inputFile.length(), Some(outputFile.length()), feedback = None)

      case Failure(ex) =>
        //ex.printStackTrace()

        // Otherwise there are empty files (or with some bytes) left over by OpenExi
        // which in turn give another error while decoding
        if (outputFile.exists()) {
          outputFile.delete()
        }
        EncodeResult(inputFile, outputFile, settings, ok = false, inputFile.length(), outputFileSize = None, feedback = Some(s"Error: ${ex.getMessage})"))
    }

    // Always write the properties file (even in case of a failure)
    result.writeAsProperties()

    result
  }

  def decode(inputStream: BufferedInputStream, outputStream: BufferedOutputStream, settings: Settings): Unit

  def decode(inputFile: ExiFile, outputDir: Directory, settings: Settings): DecodeResult = {
    outputDir.mkdirs()
    val outputFile = new XmlFile(outputDir, inputFile.getName + s".${library.id.toLowerCase}.xml")

    io(inputFile, outputFile)(decode(_, _, settings)) match {
      case Success(_) =>
        DecodeResult(inputFile, outputFile, settings, ok = true, inputFile.length(), Some(outputFile.length()), feedback = None)

      case Failure(ex) =>
        DecodeResult(inputFile, outputFile, settings, ok = false, inputFile.length(), outputFileSize = None, feedback = Some(s"Error: ${ex.getMessage}"))
    }
  }

  def validate(inputFile: ExiFile, settings: Settings): ValidateResult

  protected def validate(inputFile: ExiFile, settings: Settings, source: Source, xsdFile: XsdFile): ValidateResult = {
    val schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI)
    val schema = schemaFactory.newSchema(xsdFile)
    val validator = schema.newValidator()
    Try(validator.validate(source)) match {
      case Success(_) =>
        ValidateResult(inputFile, settings, ok = true, inputFile.length(), feedback = Some("Validation OK"))
      case Failure(ex) =>
        ValidateResult(inputFile, settings, ok = false, inputFile.length(), feedback = Some(s"Validation failed: ${ex.getMessage}"))
    }
  }
}

trait RealExiLibraryImpl extends LibraryImpl {
  final override def supports(codingMode: CodingMode, fidelityOptionMode: FidelityOptionMode): Boolean = {
    List(BytePacked, BitPacked, PreCompression, Compression).contains(codingMode) &&
      List(Strict, All, Customized).contains(fidelityOptionMode)
  }
}

case class EncodeResult(inputFile: XmlFile,
                        outputFile: ExiFile,
                        settings: Settings,
                        ok: Boolean,
                        inputFileSize: Long,
                        outputFileSize: Option[Long],
                        feedback: Option[String]) extends ResourceManager {

  def formatted(fileWidth: Int, duration: Duration): String = {
    val ratioFormatted = if (ok) {
      outputFileSize.map { ofs =>
        if (inputFileSize != 0) {
          val ratio = 100F - (ofs * 100F / inputFileSize)
          new java.text.DecimalFormat("##").format(ratio)
        } else {
          ""
        }
      }.getOrElse("?")
    } else {
      ""
    }

    f"${outputFile.getPath.padTo(fileWidth, ' ')} $inputFileSize%12s ${outputFileSize.getOrElse("")}%12s $ratioFormatted%2s%% ${settings.library}%-11s ${settings.codingMode}%-15s ${settings.fidelityOptionMode}%-13s ${settings.formattedFidelityOptions()} ${settings.xsdFile.map(_ => "Y").getOrElse("-")} ${duration.toMillis}%10s ${feedback.getOrElse("")}"
  }

  def writeAsProperties() {
    val props = new Properties()
    props.put("library", settings.library.id)

    props.put("inputFile.path", inputFile.getPath)
    props.put("inputFile.absolutePath", inputFile.getAbsolutePath)
    props.put("outputFile", outputFile.getPath)

    props.put("xsdFile.path", settings.xsdFile.map(_.getPath).getOrElse(""))
    props.put("xsdFile.absolutePath", settings.xsdFile.map(_.getAbsolutePath).getOrElse(""))
    props.put("codingMode", settings.codingMode.id)
    props.put("fidelityOptionMode", settings.fidelityOptionMode.id)

    settings.fidelityOptions.intoProperties(props)

    withResource(new BufferedWriter(new FileWriter(new File(outputFile.getPath + ".properties")))) { w =>
      props.store(w, "ExiUtils")
    }
  }
}

object EncodeResult {
  def header(fileWidth: Int): String = {
    val tmp = "File".padTo(fileWidth, ' ')
    f"$tmp ${"Size"}%12s ${"Enc size"}%12s ${"%"}%3s ${"Library"}%-11s ${"Coding mode"}%-15s ${"FidOM"}%-13s CIDPL S ${"T [ms]"}%10s"
  }
}

case class DecodeResult(inputFile: XmlFile,
                        outputFile: ExiFile,
                        settings: Settings,
                        ok: Boolean,
                        inputFileSize: Long,
                        outputFileSize: Option[Long],
                        feedback: Option[String]) {

  val baseFileName = {
    val pos = outputFile.getPath.indexOf(".xml")
    outputFile.getPath.substring(0, pos)
  }

  def formatted(fileWidth: Int, duration: Duration): String = {
    // As in EncodeResult, without ratio
    f"${outputFile.getPath.padTo(fileWidth, ' ')} $inputFileSize%12s ${outputFileSize.getOrElse("")}%12s ${settings.library}%-11s ${settings.codingMode}%-15s ${settings.fidelityOptionMode}%-13s ${settings.formattedFidelityOptions()} ${settings.xsdFile.map(_ => "Y").getOrElse("-")} ${duration.toMillis}%10s ${feedback.getOrElse("")}"
  }
}

object DecodeResult {
  def header(fileWidth: Int): String = {
    // As in EncodeResult, without ratio; swap "Size" and "Enc size"
    val tmp = "File".padTo(fileWidth, ' ')
    f"$tmp ${"Enc size"}%12s ${"Size"}%12s ${"Library"}%-11s ${"Coding mode"}%-15s ${"FidOM"}%-13s CIDPL S ${"T [ms]"}%10s"
  }

}

case class ValidateResult(inputFile: ExiFile,
                          settings: Settings,
                          ok: Boolean,
                          inputFileSize: Long,
                          feedback: Option[String]) {

  val baseFileName = {
    val pos = inputFile.getPath.indexOf(".xml")
    inputFile.getPath.substring(0, pos)
  }

  def formatted(fileWidth: Int, duration: Duration): String = {
    // As in DecodeResult, using inputFile instead of outputFile; only input file size
    f"${inputFile.getPath.padTo(fileWidth, ' ')} $inputFileSize%12s ${settings.library}%-11s ${settings.codingMode}%-15s ${settings.fidelityOptionMode}%-13s ${settings.formattedFidelityOptions()} ${settings.xsdFile.map(_ => "Y").getOrElse("-")} ${duration.toMillis}%10s ${feedback.getOrElse("")}"
  }
}

object ValidateResult {
  def header(fileWidth: Int): String = {
    val tmp = "File".padTo(fileWidth, ' ')
    f"$tmp ${"Enc size"}%12s ${"Library"}%-11s ${"Coding mode"}%-15s ${"FidOM"}%-13s CIDPL S ${"T [ms]"}%10s"
  }
}
