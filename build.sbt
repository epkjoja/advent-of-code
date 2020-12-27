name := "AdventOfCode"

version := "0.1"

scalaVersion := "2.13.4"

libraryDependencies ++= Seq(
  "org.scalanlp"  %% "breeze"          % "1.1",
  "dev.zio"       %% "zio-streams"     % "1.0.3",
  "org.parboiled" %% "parboiled-scala" % "1.3.1"
)
