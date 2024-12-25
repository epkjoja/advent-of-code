name := "AdventOfCode"

version := "0.1"

scalaVersion := "2.13.15"

libraryDependencies ++= Seq(
  "org.scalanlp"  %% "breeze"          % "2.1.0",
  "co.fs2"        %% "fs2-core"        % "3.11.0",
  "co.fs2"        %% "fs2-io"          % "3.11.0",
  "org.parboiled" %% "parboiled-scala" % "1.4.1",
  "org.scalactic" %% "scalactic"       % "3.2.19",
  "org.scalatest" %% "scalatest"       % "3.2.19" % "test"
)
