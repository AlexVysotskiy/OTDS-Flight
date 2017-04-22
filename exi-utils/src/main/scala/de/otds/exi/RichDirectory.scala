package de.otds.exi

import java.io.{File, IOException}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

object RichDirectoryContainer {

  implicit class RichDirectory(val directory: File) extends AnyVal {
    def deleteRecursively(): Unit = {
      Files.walkFileTree(directory.toPath, new SimpleFileVisitor[Path]() {
        override def visitFile(file: Path, attrs: BasicFileAttributes) = {
          Files.delete(file)
          FileVisitResult.CONTINUE
        }

        override def postVisitDirectory(dir: Path, exc: IOException) ={
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }
      })

    }

  }

}