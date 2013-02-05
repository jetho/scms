scalaVersion := "2.10.0-M7"
 
resolvers += "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/"
 
libraryDependencies ++= Seq(
  "org.scalaz" % "scalaz-core" % "7.0.0-M3" cross CrossVersion.full, 
  "org.specs2" % "specs2_2.10.0-M7" % "1.12.1"
)
 
scalacOptions += "-feature"
 
initialCommands in console := "import scalaz._, Scalaz._, org.jetho.scms._"
