package de.otds.exi

import java.io.File
import java.net.URL
import RichUrlContainer._
import RichFileContainer._

trait TestData {
  import TestData.CarParking._

  def setupTestData(): Unit = {
    println("= Setting up test data =")
    println("")

    if (TestDataDirectory.exists() == false || InputFile.exists() == false || XsdFile.exists() == false) {
      TestDataDirectory.mkdirs()

      if (XsdFile.exists() == false) {
        val url = new URL("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/399279/carparking.xsd")
        println(s"Downloading $url")
        url.download(new File(TestDataDirectory, "carparking.xsd"))
      }

      val zipFile = new File(TestDataDirectory, "carparking.zip")
      if (zipFile.exists() == false) {
        val url = new URL("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/388117/IF064_052_201412160800.zip")
        println(s"Downloading $url")
        url.download(zipFile)
      }

      if (InputFile.exists() == false || InputFile.length() == 0) {
        println(s"Unzipping $zipFile")
        zipFile.unzip(TestDataDirectory)
      }

      println(s"Test data has been installed in $TestDataDirectory")
    } else {
      println(s"Test data was already installed in $TestDataDirectory")
    }
  } ensuring(InputFile.exists() && XsdFile.exists())
}

object TestData {
  object CarParking {
    protected[TestData] val TestDataDirectory = new Directory("data/sample-data/car-parking")

    val XsdFile = new File(TestDataDirectory, "carparking.xsd")

    val InputDirectory = TestDataDirectory

    val InputFile = new File(TestDataDirectory, "CarParkData_1.xml")
  }

  object Persons {
    private val TestDataDirectory = new Directory("src/test/resources/files/persons")

    val XsdFile = new File(TestDataDirectory, "persons.xsd")

    val InputDirectory = TestDataDirectory

    val InputFile = new File(TestDataDirectory, "persons.xml")
  }
}
