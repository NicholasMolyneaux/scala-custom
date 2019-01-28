name := "scala-custom"

organization := "com.github.NicholasMolyneaux"

homepage := Some(url("https://github.com/NicholasMolyneaux/scala-custom"))
scmInfo := Some(ScmInfo(url("https://github.com/NicholasMolyneaux/scala-custom"),"git@github.com:NicholasMolyneaux/scala-custom.git"))
developers := List(Developer("NicholasMolyneaux",
  "Nicholas Molyneaux",
  "nicolas.molyneaux@gmail.com",
  url("https://github.com/NicholasMolyneaux")))
licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
publishMavenStyle := true
publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)
version := "1.1.4"

scalaVersion := "2.12.4"

// play framework, needed for JSON parser
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.7"

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

libraryDependencies += "junit" % "junit" % "4.11" % "test"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.13"

libraryDependencies += "org.scalanlp" %% "breeze-natives" % "0.13"
