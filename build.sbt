name := "AdventOfCode"

version := "0.1"

scalaVersion := "2.13.7"

libraryDependencies ++= Seq(
  "org.scalanlp"  %% "breeze"          % "1.3",
  "dev.zio"       %% "zio-streams"     % "1.0.12",
  "org.parboiled" %% "parboiled-scala" % "1.3.1"
)
