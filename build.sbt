name := "AdventOfCode"

version := "0.1"

scalaVersion := "2.13.10"

libraryDependencies ++= Seq(
  "org.scalanlp"  %% "breeze"          % "2.1.0",
  "dev.zio"       %% "zio-streams"     % "2.0.4",
  "org.parboiled" %% "parboiled-scala" % "1.4.1"
)
