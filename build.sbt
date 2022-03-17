ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "scala-training-itechart"
  )

libraryDependencies += "org.scalatest"    %% "scalatest"   % "3.1.1" % "test"
libraryDependencies += "com.github.scopt" %% "scopt"       % "4.0.1"
libraryDependencies += "org.typelevel"    %% "cats-effect" % "3.3.7"
libraryDependencies ++= Seq(
  "eu.timepit" %% "refined"            % "0.9.28",
  "eu.timepit" %% "refined-cats"       % "0.9.28", // optional
  "eu.timepit" %% "refined-eval"       % "0.9.28", // optional, JVM-only
  "eu.timepit" %% "refined-jsonpath"   % "0.9.28", // optional, JVM-only
  "eu.timepit" %% "refined-pureconfig" % "0.9.28", // optional, JVM-only
  "eu.timepit" %% "refined-scalacheck" % "0.9.28", // optional
  "eu.timepit" %% "refined-scalaz"     % "0.9.28", // optional
  "eu.timepit" %% "refined-scodec"     % "0.9.28", // optional
  "eu.timepit" %% "refined-scopt"      % "0.9.28", // optional
  "eu.timepit" %% "refined-shapeless"  % "0.9.28" // optional
)
