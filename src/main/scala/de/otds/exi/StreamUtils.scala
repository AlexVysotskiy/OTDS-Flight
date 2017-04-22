package de.otds.exi

import java.io.{InputStream, OutputStream}

trait StreamUtils {
  def copyStream(inputStream: InputStream, outputStream: OutputStream, bufferSize: Int = 512): Int = {
    // TODO C Stream.continually(zis.read(buffer)).takeWhile(_ != -1).foreach(out.write(buffer, 0, _))

    val buf = Array.ofDim[Byte](bufferSize)
    var totalRead = 0
    var read: Int = 1
    while (read > 0) {
      read = inputStream.read(buf, 0, bufferSize)
      if (read > 0) {
        totalRead = totalRead + read
        outputStream.write(buf, 0, read)
      }
    }

    totalRead
  }
}
