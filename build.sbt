name := "scms"

version := "1.0"

organization := "org.jetho"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq("org.specs2" %% "specs2" % "1.7.1" % "test")

resolvers ++= Seq("snapshots" at "http://scala-tools.org/repo-snapshots", "releases"  at "http://scala-tools.org/repo-releases")
