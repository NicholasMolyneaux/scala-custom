name := "scala-custom"

organization := "transpor.molyneaux"

version := "1.1.6.2"

scalaVersion := "2.11.8"

// play framework, needed for JSON parser
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.5.12"
resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "junit" % "junit" % "4.11" % "test"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.12"
libraryDependencies += "org.scalanlp" %% "breeze-natives" % "0.12"