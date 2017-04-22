package de.otds.exi

import sys.process._
import java.net.URL
import java.io.File

import scala.language.postfixOps

object RichUrlContainer {
  implicit class RichUrl(val url: URL) extends AnyVal {
    def download(file: File): Unit = {
      url #> file !!
    }
  }
}
