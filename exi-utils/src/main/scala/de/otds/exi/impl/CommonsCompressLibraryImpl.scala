package de.otds.exi.impl

import java.io.{BufferedInputStream, BufferedOutputStream, FileInputStream}
import javax.xml.transform.stream.StreamSource

import de.otds.exi._
import org.apache.commons.compress.compressors.CompressorStreamFactory

abstract class CommonsCompressLibraryImpl(name: String) extends LibraryImpl with StreamUtils {

  override def supports(codingMode: CodingMode, fidelityOptionMode: FidelityOptionMode): Boolean = {
    codingMode == Default && fidelityOptionMode == NotApplicable
  }

  override def encode(inputStream: BufferedInputStream, outputStream: BufferedOutputStream, settings: Settings): Unit = {
    val factory = new CompressorStreamFactory()
    withResource(factory.createCompressorOutputStream(name, outputStream)) { os =>
      copyStream(inputStream, os)
    }
  }

  override def decode(inputStream: BufferedInputStream, outputStream: BufferedOutputStream, settings: Settings): Unit = {
    val factory = new CompressorStreamFactory()
    withResource(factory.createCompressorInputStream(inputStream)) { is =>
      copyStream(is, outputStream)
    }
  }

  override def validate(inputFile: ExiFile, settings: Settings): ValidateResult = {
    settings.xsdFile match {
      case Some(xsdFile) =>
        val factory = new CompressorStreamFactory()
        withResource(factory.createCompressorInputStream(new BufferedInputStream(new FileInputStream(inputFile)))) {
          inputStream =>
            val source = new StreamSource(inputStream)
            validate(inputFile, settings, source, xsdFile)
        }

      case None =>
        ValidateResult(inputFile, settings, ok = false, inputFile.length(), feedback = Some("Cannot validate without .xsd file"))
    }
  }
}
