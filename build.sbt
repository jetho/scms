scalaVersion := "2.10.0"
 
resolvers += "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/"
 
libraryDependencies ++= Seq(
  "org.scalaz"  %% "scalaz-core" % "7.0.0-M8",
  "org.specs2" %% "specs2" % "1.12.3" % "test"
)
 
scalacOptions += "-feature"
 
initialCommands in console := "import scalaz._, Scalaz._, org.jetho.scms._"
