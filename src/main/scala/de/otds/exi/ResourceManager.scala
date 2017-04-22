package de.otds.exi

import java.io.{Closeable, File}
import java.nio.file.Files

import de.otds.exi.RichDirectoryContainer._

trait ResourceManager {
  def withResource[T <: Closeable, R](r: T)(f: T => R): R = {
    try {
      f(r)
    } finally {
      r.close()
    }
  }

  def withTmpDirectory[R](f: File => R): R = {
    val dir = Files.createTempDirectory("").toFile
    try {
      f(dir)
    } finally {
      dir.deleteRecursively()
    }
  }
}
