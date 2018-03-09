name := "scala-custom"

organization := "transpor.molyneaux"

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.4"

// play framework, needed for JSON parser
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.7"
resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

libraryDependencies += "junit" % "junit" % "4.11" % "test"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.13"
libraryDependencies += "org.scalanlp" %% "breeze-natives" % "0.13"