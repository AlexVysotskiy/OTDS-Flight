name := "exi-utils"

// See also
//   exi/package.scala, val Version
//   exi-utils script
//   exi-utils.bat
version := "1.1"

scalaVersion := "2.11.8"

organization := "de.otds.exi"

scalacOptions ++= Seq("-feature", "-deprecation", "-Xfatal-warnings")

// OpenExi is not multi-thread safe, so parallel tests must be disabled
// http://www.scala-sbt.org/0.13/docs/Testing.html
// By default, sbt runs all tasks in parallel and within the same JVM as sbt itself.
parallelExecution in Test := false

libraryDependencies += "com.siemens.ct.exi" % "exificient" % "0.9.6"

libraryDependencies += "org.rogach" % "scallop_2.11" % "2.1.1"

libraryDependencies += "org.apache.commons" % "commons-compress" % "1.13"

// Optional dependency of commons-compress!
libraryDependencies += "org.tukaani" % "xz" % "1.6"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
