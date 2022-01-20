course := "effective-scala"
assignment := "codecs"

scalaVersion := "3.1.0"
scalacOptions ++= Seq("-deprecation")
libraryDependencies ++= Seq(
  "org.typelevel" %% "jawn-parser"       % "1.1.2",
  "org.scalameta"  %% "munit"            % "0.7.26"  % Test,
  "org.scalameta"  %% "munit-scalacheck" % "0.7.26"  % Test
)
