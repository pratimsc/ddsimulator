import AssemblyKeys._ // put this at the top of the file

assemblySettings

name := "ddSimulator"

version := "0.1.0"

scalaVersion := "2.10.2"

exportJars := true

jarName in assembly := "ddSimulator." + version

scalacOptions ++= Seq("-deprecation", "-feature","-target:jvm-1.6")

libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "1.9.1" % "test",
	"junit" % "junit" % "4.10" % "test",
	"org.slf4j" % "slf4j-api" % "1.7.5",
	"ch.qos.logback" % "logback-core" % "1.0.12",
	"ch.qos.logback" % "logback-classic" % "1.0.12",
	"joda-time" % "joda-time" % "2.2",
	"org.joda" % "joda-convert" % "1.3.1")
