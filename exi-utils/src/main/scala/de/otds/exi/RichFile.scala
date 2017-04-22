package de.otds.exi

import java.io._
import java.util.zip.ZipInputStream

object RichFileContainer {
  // TODO C AnyVal
  implicit class RichFile(val file: File) extends /* AnyVal with */ ResourceManager with StreamUtils {
    /**
      * Expands a java.io.File to itself (if it is a file) or to the files in the directory which match the given extension.
      *
      * @param ext The file extension
      * @return A list of files
      */
    def expand(ext: String): List[File] = {
      if (file.isFile) {
        file :: Nil
      } else {
        file.listFiles().toList.filter(_.getName.endsWith(ext))
      }
    }

    // TODO C Return : Iterable[File]
    def unzip(targetDirectory: Directory) = {
      withResource(new ZipInputStream(new FileInputStream(file))) { zis =>
        // TODO C flatMap does not work - foreach is required!
        Stream.continually(zis.getNextEntry).takeWhile(_ != null).foreach { file =>
          println(s"  Extracting $file")
          if (file.isDirectory == false) {
            val outputFile = new File(targetDirectory.getPath, file.getName)
            outputFile.getParentFile.mkdirs()

            withResource(new BufferedOutputStream(new FileOutputStream(outputFile))) { os =>
              copyStream(zis, os)
            }

            Some(outputFile)
          } else {
            None
          }
        }
      }
    }
  }
}