package de.otds.exi.impl

import java.io.{BufferedInputStream, BufferedOutputStream, File, FileInputStream}
import javax.xml.transform.stream.StreamResult
import javax.xml.transform.{Source, TransformerFactory}

import com.siemens.ct.exi.api.sax.{EXIResult, EXISource}
import com.siemens.ct.exi.helpers.DefaultEXIFactory
import com.siemens.ct.exi.{EXIFactory, FidelityOptions, GrammarFactory}
import de.otds.exi._
import org.xml.sax.InputSource
import org.xml.sax.helpers.XMLReaderFactory

import scala.language.implicitConversions

class ExificientLibraryImpl extends RealExiLibraryImpl {
  override val library = Exificient

  private implicit def toExificientCodingMode(alignment: CodingMode): com.siemens.ct.exi.CodingMode = {
    alignment match {
      case BitPacked =>
        com.siemens.ct.exi.CodingMode.BIT_PACKED

      case BytePacked =>
        com.siemens.ct.exi.CodingMode.BYTE_PACKED

      case PreCompression =>
        com.siemens.ct.exi.CodingMode.PRE_COMPRESSION

      case Compression =>
        com.siemens.ct.exi.CodingMode.COMPRESSION

      case Default | Size | Speed =>
        com.siemens.ct.exi.CodingMode.COMPRESSION
    }
  }

  private def createExiFactory(settings: Settings): EXIFactory = {
    val exiFactory = DefaultEXIFactory.newInstance()
    exiFactory.setCodingMode(settings.codingMode)

    val fo = {
      settings.fidelityOptionMode match {
        case Strict =>
          val fo = FidelityOptions.createStrict()
          fo.setFidelity(FidelityOptions.FEATURE_LEXICAL_VALUE, settings.fidelityOptions.preserveLexicalValues)
          fo

        case All =>
          FidelityOptions.createAll()

        case Customized =>
          val fo = FidelityOptions.createDefault()
          fo.setFidelity(FidelityOptions.FEATURE_COMMENT, settings.fidelityOptions.preserveComments)
          fo.setFidelity(FidelityOptions.FEATURE_PI, settings.fidelityOptions.preserveProcessingInstructions)
          fo.setFidelity(FidelityOptions.FEATURE_DTD, settings.fidelityOptions.preserveDtdsAndEntityReferences)
          fo.setFidelity(FidelityOptions.FEATURE_PREFIX, settings.fidelityOptions.preservePrefixes)
          fo.setFidelity(FidelityOptions.FEATURE_LEXICAL_VALUE, settings.fidelityOptions.preserveLexicalValues)
          fo

        case NotApplicable =>
          FidelityOptions.createStrict()
      }
    }
    exiFactory.setFidelityOptions(fo)

    settings.xsdFile.foreach { f =>
      val gf = GrammarFactory.newInstance()
      exiFactory.setGrammars(gf.createGrammars(f.getPath))
    }

    exiFactory
  }

  override def encode(inputStream: BufferedInputStream, outputStream: BufferedOutputStream, settings: Settings): Unit = {
    val exiFactory = createExiFactory(settings)
    val exiResult = new EXIResult(exiFactory)
    exiResult.setOutputStream(outputStream)

    val xmlReader = XMLReaderFactory.createXMLReader()
    xmlReader.setContentHandler(exiResult.getHandler)
    xmlReader.parse(new InputSource(inputStream))
  }

  override def decode(inputStream: BufferedInputStream, outputStream: BufferedOutputStream, settings: Settings): Unit = {
    val inputSource = new InputSource(inputStream)

    val exiFactory = createExiFactory(settings)
    val saxSource = new EXISource(exiFactory)
    saxSource.setInputSource(inputSource)

    val tf = TransformerFactory.newInstance()
    val transformer = tf.newTransformer()
    val result = new StreamResult(outputStream)
    transformer.transform(saxSource, result)
  }

  private def getSourceForValidation(inputFile: File, settings: Settings, inputStream: BufferedInputStream): Source = {
    val exiFactory = createExiFactory(settings)
    val exiSource = new EXISource(exiFactory)
    exiSource.setInputSource(new InputSource(inputStream))
    exiSource
  }

  override def validate(inputFile: ExiFile, settings: Settings): ValidateResult = {
    settings.xsdFile match {
      case Some(xsdFile) if xsdFile.canRead =>
        withResource(new BufferedInputStream(new FileInputStream(inputFile))) {
          inputStream =>
            val source = getSourceForValidation(inputFile, settings, inputStream)
            validate(inputFile, settings, source, xsdFile)
        }

      case Some(xsdFile) if xsdFile.canRead == false =>
        ValidateResult(inputFile, settings, ok = false, inputFile.length(), feedback = Some(s"Cannot read xsd file $xsdFile"))

      case None =>
        ValidateResult(inputFile, settings, ok = false, inputFile.length(), feedback = Some("Cannot validate without .xsd file"))
    }
  }
}
