scalaVersion := "2.11.7"

// scalaz
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.5"
libraryDependencies += "org.scalaz" %% "scalaz-scalacheck-binding" % "7.1.5" % "test"

// specs2
libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.6.5" % "test")
libraryDependencies ++= Seq("org.specs2" %% "specs2-scalacheck" % "3.6.5" % "test")
scalacOptions in Test ++= Seq("-Yrangepos")

scalacOptions ++= Seq("-feature","-language:higherKinds")