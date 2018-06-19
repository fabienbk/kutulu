import java.nio.charset.Charset

name := "kutulu"

version := "0.1"

scalaVersion := "2.12.6"

// https://mvnrepository.com/artifact/org.scalatest/scalatest
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test

sourceGenerators in Compile += Def.task {

  val outputFile = (sourceManaged in Compile).value / "App.scala"

  val sourceRoot  = baseDirectory.value / "src" / "main" / "scala"
  println(sourceRoot)

  IO.write(outputFile, "package output\n\n")
  for (inputFile <- sourceRoot.listFiles())  {
    IO.write(outputFile, IO.read(inputFile), Charset.defaultCharset(), true)
  }

  Seq(outputFile)

}
