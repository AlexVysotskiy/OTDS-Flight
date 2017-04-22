package de.otds.exi.impl

import java.io.{BufferedInputStream, BufferedOutputStream, File, FileInputStream}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import javax.xml.XMLConstants
import javax.xml.transform.sax.SAXSource
import javax.xml.transform.stream.{StreamResult, StreamSource}
import javax.xml.transform.{Source, TransformerFactory}
import javax.xml.validation.SchemaFactory

import com.siemens.ct.exi.api.sax.{EXIResult, EXISource}
import com.siemens.ct.exi.helpers.DefaultEXIFactory
import com.siemens.ct.exi.{EXIFactory, FidelityOptions, GrammarFactory}
import de.otds.exi._
import org.xml.sax.InputSource
import org.xml.sax.helpers.XMLReaderFactory

import scala.util.{Failure, Success, Try}

class GzipLibraryImpl extends LibraryImpl with StreamUtils {
  override val library = Gzip

  override def supports(codingMode: CodingMode, fidelityOptionMode: FidelityOptionMode): Boolean = {
    codingMode == Default && fidelityOptionMode == NotApplicable
  }

  override def encode(inputStream: BufferedInputStream, outputStream: BufferedOutputStream, settings: Settings): Unit = {
    withResource(new GZIPOutputStream(outputStream)) { os =>
      copyStream(inputStream, os)
    }
  }

  override def decode(inputStream: BufferedInputStream, outputStream: BufferedOutputStream, settings: Settings): Unit = {
    withResource(new GZIPInputStream(inputStream)) { is =>
      copyStream(is, outputStream)
    }
  }

  override def validate(inputFile: ExiFile, settings: Settings): ValidateResult = {
    settings.xsdFile match {
      case Some(xsdFile) =>
        withResource(new GZIPInputStream(new BufferedInputStream(new FileInputStream(inputFile)))) {
          inputStream =>
            val source = new StreamSource(inputStream)
            validate(inputFile, settings, source, xsdFile)
        }

      case None =>
        ValidateResult(inputFile, settings, ok = false, inputFile.length(), feedback = Some("Cannot validate without .xsd file"))
    }
  }
}
