name := "alternacat"

version := "0.1"

scalaVersion := "2.12.4"

mainClass in (Compile, run) := Some("consoleApplication.Driver")

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.4" % "test"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.9"
libraryDependencies += "org.rogach" %% "scallop" % "3.1.3"
libraryDependencies += "org.apache.lucene" % "lucene-core" % "7.4.0"
libraryDependencies += "org.apache.lucene" % "lucene-queryparser" % "7.4.0"
libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "2.1.0"
libraryDependencies += "com.typesafe" % "config" % "1.3.2"
libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.11.1"
libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.11.1"
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.1"
