package de.otds.exi

import java.io.File

import de.otds.exi.RichFileContainer._
import de.otds.exi.impl.{DecodeResult, EncodeResult, ValidateResult}
import org.rogach.scallop._

import scala.concurrent.duration.Duration
import scala.language.{implicitConversions, reflectiveCalls}

class Args(args: Array[String]) extends ScallopConf(args) {
  version(s"${App.Name} ${App.Version}")

  val verbose = toggle(name = "verbose", short = 'v')

  banner(
    """
      |usage: exi-utils [--version] [--help] <sub-command> <args>
    """.stripMargin)

  val testData = new Subcommand("testdata") {
    val download = toggle(name = "download", short = 'd')
    banner("Download test data")
    footer("")
  }
  addSubcommand(testData)

  val encode = new Subcommand("encode") {
    val libraryIds = opt[String](name = "libraries", short = 'l', required = true, default = Some(Library.Values.map(_.id).mkString(",")))

    val outputDirectory = opt[File](name = "output-directory", short = 'o', required = true, default = Some(new File("/tmp")))

    val xsdFile = opt[File](name = "xsd-file", short = 'x')

    val codingModeIds = opt[String](name = "coding-modes", short = 'c', required = true, default = Some(CodingMode.Values.map(_.id).mkString(",")))

    val fidelityOptionModeIds = opt[String](name = "fidelity-option-modes", short = 'f', required = true, default = Some(FidelityOptionMode.Values.map(_.id).mkString(",")))

    val preserveComments = toggle(name = "preserve-comments", short = 'C')
    val preserveProcessingInstructions = toggle(name = "preserve-processing-instructions", short = 'I')
    val preserveDtdsAndEntityReferences = toggle(name = "preserve-dtds-and-entity-references", short = 'D')
    val preservePrefixes = toggle(name = "preserve-prefixes", short = 'P')
    val preserveLexicalValues = toggle(name = "preserve-lexical-values", short = 'L')

    val fileNames = trailArg[List[String]]()

    def libraries(): List[Library] = libraryIds().split(",").flatMap(id => Library(id)).toList

    def codingModes(): List[CodingMode] = codingModeIds().split(",").flatMap(id => CodingMode(id)).toList

    def fidelityOptionModes(): List[FidelityOptionMode] = fidelityOptionModeIds().split(",").flatMap(id => FidelityOptionMode(id)).toList

    def rawFiles(): List[File] = fileNames().map(f => new File(f))
    def files(): List[File] = rawFiles().flatMap(_.expand(".xml"))

    banner("Encode files")
    footer(
      s"""
        |Supported libraries: ${Library.Values.map(_.id).mkString(", ")}
        |Supported coding modes: ${CodingMode.Values.map(_.id).mkString(", ")}
        |Supported fidelity options: ${FidelityOptionMode.Values.map(_.id).mkString(", ")}
        |
        |""".stripMargin)
  }
  addSubcommand(encode)

  val decode = new Subcommand("decode") {
    val outputDirectory = opt[File](name = "output-directory", short = 'o', required = true, default = Some(new File("/tmp")))

    val fileNames = trailArg[List[String]]()

    def rawFiles(): List[File] = fileNames().map(f => new File(f))
    def files(): List[File] = rawFiles().flatMap(_.expand(".exi"))

    banner("Decode files")
    footer("")
  }
  addSubcommand(decode)

  val validate = new Subcommand("validate") {
    val fileNames = trailArg[List[String]]()

    def rawFiles(): List[File] = fileNames().map(f => new File(f))
    def files(): List[File] = rawFiles().flatMap(_.expand(".exi"))

    banner("Validate encoded files")
    footer("")
  }
  addSubcommand(validate)

  verify()
}

object ExiUseCases extends Timing with ResourceManager with TestData {
  def testData(args: Args) = {
    if (args.testData.download.isDefined) {
      setupTestData()
    }
  }

  def encode(args: Args): List[(EncodeResult, Duration)] = {
    checkXsdFile(args.encode.xsdFile.toOption)
    checkFiles(args.encode.rawFiles())

    val results = args.encode.libraries().zipWithIndex.flatMap { case (lib, libIndex) =>
      println(s"= Encode using ${lib.id} =")
      val impl = lib.instance()

      val (libResults, libDuration) = measure {
        args.encode.files().flatMap { f =>
          for {
            cm <- args.encode.codingModes()
            fom <- args.encode.fidelityOptionModes()
            if impl.supports(cm, fom)
          } yield {
            print(s"  Encoding $f ... ")
            val (result, duration) = measure {
              val cfo =
                FidelityOptions(
                  args.encode.preserveComments.toOption.getOrElse(false),
                  args.encode.preserveProcessingInstructions.toOption.getOrElse(false),
                  args.encode.preserveDtdsAndEntityReferences.toOption.getOrElse(false),
                  args.encode.preservePrefixes.toOption.getOrElse(false),
                  args.encode.preserveLexicalValues.toOption.getOrElse(false))
              impl.encode(f, args.encode.outputDirectory(), Settings(lib, args.encode.xsdFile.toOption, cm, fom, cfo))
            }

            if (args.verbose.isDefined) {
              println(s"$duration ${if (result.ok) "OK" else s"FAILED: ${result.settings}"}")
            } else {
              println(s"$duration")
            }

            (result, duration)
          }
        }
      }

      println()
      println(s"$lib duration: $libDuration")
      println()

      libResults
    }

    if (results.nonEmpty) {
      val (okResults, errorResults) = results.partition { case (result, _) => result.ok }
      val width = results.map { case (result, _) => result.outputFile.getPath.length }.max

      println("= Results = ")
      println()

      println("== By size ==")
      println()
      println(EncodeResult.header(width))
      // Group by input file!
      // Sort by library ID: If there is no .xsd, the files will have the same size!
      okResults
        .sortBy { case (result, _) => (result.inputFile, result.outputFileSize, result.settings.library.id) }
        .foreach { case (result, duration) =>
          println(result.formatted(width, duration))
        }

      println()

      println("== By duration ==")
      println()
      println(EncodeResult.header(width))
      okResults
        .sortBy { case (result, duration) => (result.inputFile, duration, result.settings.library.id) }
        .foreach { case (result, duration) =>
          println(result.formatted(width, duration))
        }

      if (errorResults.nonEmpty) {
        println()
        println("= Errors =")
        println()
        errorResults
          .sortBy { case (result, _) => result.outputFile }
          .foreach { case (result, duration) =>
            println(result.formatted(width, duration))
          }
      }
    }

    results
  }

  def decode(args: Args): List[(DecodeResult, Duration)] = {
    checkFiles(args.decode.rawFiles())

    case class Param(inputFile: File, settings: Settings)

    val params = args.decode.files().map { f =>
      val settings = Settings(new File(f + ".properties"))
      Param(f, settings)
    }

    val results = params.groupBy(_.settings.library).toList.flatMap { case (library, ps) =>
      println(s"= Decode using $library =")
      val (libResults, libDuration) = measure {
        val impl = library.instance()
        val result = ps.map { case Param(f, settings) =>
          print(s"  Decoding $f ... ")
          val (result, duration) = measure {
            impl.decode(f, args.decode.outputDirectory(), settings)
          }
          println(s"$duration")

          (result, duration)
        }
        println()

        result
      }
      println(s"$library duration: $libDuration")
      println()

      libResults
    }

    val (okResults, errorResults) = results.partition { case (result, _) => result.ok }
    val width = results.map { case (result, _) => result.outputFile.getPath.length }.max

    println()
    println("= Results =")
    println()
    println(DecodeResult.header(width))
    okResults.
      sortBy { case (result, duration) => (result.baseFileName, duration.toMillis) }.
      foreach { case (result, duration) =>
        println(result.formatted(width, duration))
      }

    if (errorResults.nonEmpty) {
      println()
      println("= Errors =")
      println()
      errorResults.
        sortBy { case (result, _) => result.outputFile }.
        foreach { case (result, duration) =>
          println(result.formatted(width, duration))
        }
    }

    results
  }

  def validate(args: Args): List[(ValidateResult, Duration)] = {
    // TODO C Duplicate code, see decode op

    checkFiles(args.validate.rawFiles())

    case class Param(inputFile: File, settings: Settings)

    val params = args.validate.files().map { f =>
      val settings = Settings(new File(f + ".properties"))
      Param(f, settings)
    }

    val results = params.groupBy(_.settings.library).toList.flatMap { case (library, ps) =>
      println(s"= Validate using $library =")
      val (libResults, libDuration) = measure {
        val impl = library.instance()
        val result = ps.map { case Param(f, settings) =>
          print(s"  Validating $f ... ")
          val (result, duration) = measure {
            impl.validate(f, settings)
          }
          println(s"$duration")

          (result, duration)
        }
        println()

        result
      }
      println(s"$library duration: $libDuration")
      println()

      libResults
    }

    val (okResults, errorResults) = results.partition { case (result, _) => result.ok }
    val width = results.map { case (result, _) => result.inputFile.getPath.length }.max

    println()
    println("= Results =")
    println()
    println(ValidateResult.header(width))
    okResults.
      sortBy { case (result, duration) => (result.baseFileName, duration.toMillis) }.
      foreach { case (result, duration) =>
        println(result.formatted(width, duration))
      }

    if (errorResults.nonEmpty) {
      println()
      println("= Errors =")
      println()
      errorResults.
        sortBy { case (result, _) => result.inputFile }.
        foreach { case (result, duration) =>
          println(result.formatted(width, duration))
        }
    }

    results
  }

  private def checkXsdFile(xsdFile: Option[XsdFile]) {
    xsdFile.foreach { f =>
      if (f.canRead == false) {
        System.err.println(s"XSD file $f does not exist")
        System.exit(1)
      }
    }
  }

  private def checkFiles(files: List[File]): Unit = {
    files.foreach { f =>
      if (f.exists() == false) {
        System.err.println(s"File/directory $f does not exist")
        System.exit(1)
      }
    }
  }
}

object Exi extends App {
  val exiUtilsArgs = new Args(args)

  exiUtilsArgs.subcommand match {
    case Some(exiUtilsArgs.testData) =>
      ExiUseCases.testData(exiUtilsArgs)

    case Some(exiUtilsArgs.encode) =>
      ExiUseCases.encode(exiUtilsArgs)

    case Some(exiUtilsArgs.decode) =>
      ExiUseCases.decode(exiUtilsArgs)

    case Some(exiUtilsArgs.validate) =>
      ExiUseCases.validate(exiUtilsArgs)

    case _ =>
      exiUtilsArgs.printHelp()
  }
}
