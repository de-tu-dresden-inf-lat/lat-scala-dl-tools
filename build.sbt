//import de.johoop.jacoco4sbt._
//import JacocoPlugin._

//import AssemblyKeys._


// General settings
ThisBuild / name := "lat-scala-dl-tools"
ThisBuild / organization := "io.github.de-tu-dresden-inf-lat"
ThisBuild / version := "0.4.1"
ThisBuild / scalaVersion := "2.12.6" //"2.11.6"
ThisBuild / testOptions in Test := Seq(Tests.Argument(TestFrameworks.JUnit, "-a"))
ThisBuild / assemblyJarName in assembly := name.value+"-standalone.jar"
ThisBuild / test in assembly := { }
ThisBuild / assemblyMergeStrategy in assembly := {
  case PathList("net.sourceforge.owlapi", "owlapi-distribution", xs @
    _*)         => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".class" => MergeStrategy.first
  //  case PathList(ps @ _*) if ps.last endsWith ".html" => MergeStrategy.first
  //  case "application.conf"                            => MergeStrategy.concat
  //  case "unwanted.txt"                                =>
  //  MergeStrategy.discard
  case "META-INF/axiom.xml" => MergeStrategy.first
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}
ThisBuild / scalacOptions in (Compile, doc) ++= Seq("-skip-packages", "uk.ac.man.cs.lethe.internal:uk.ac.man.cs.lethe.klappoExperiments")
ThisBuild / libraryDependencies ++= Seq(
  // for unit tests
  "org.scalatest" %% "scalatest" % "3.0.5",
  "com.novocode" % "junit-interface" % "0.11",

  //  "org.scala-lang.modules" %% "scala-swing" % "2.1.1",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",

  // logging related
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
)

lazy val lat_scala_dl_tools_owlapi4 = Project(
  id = "lat-scala-dl-tools-owlapi4",
  base = file("lat-scala-dl-tools-owlapi4"))
  .settings(
    name := "lat-scala-dl-tools-owlapi4",
    libraryDependencies += "net.sourceforge.owlapi" % "owlapi-distribution" % "4.5.19"
  )


lazy val lat_scala_dl_tools_owlapi5 = Project(
  id = "lat-scala-dl-tools-owlapi5",
  base = file("lat-scala-dl-tools-owlapi5"))
  .settings(
    name := "lat-scala-dl-tools-owlapi5",
    libraryDependencies += "net.sourceforge.owlapi" % "owlapi-distribution" % "5.1.17",
    scalaSource in Compile := file(file("lat-scala-dl-tools-owlapi4/src/main/scala").getAbsolutePath),
  )
