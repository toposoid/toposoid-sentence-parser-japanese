import Dependencies._

ThisBuild / scalaVersion     := "2.12.12"
ThisBuild / version          := "0.1.1"
ThisBuild / organization     := "com.ideal.linked"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full)
scalacOptions += "-Ypartial-unification"

lazy val root = (project in file("."))
  .settings(
    name := "toposoid-sentence-parser-japanese",
    libraryDependencies ++= Seq(
      "com.enjapan" %% "scala-juman-knp" % "0.0.10-SNAPSHOT",
      "org.typelevel" %% "cats-core" % "0.8.1"
    ),
    dependencyOverrides += "org.typelevel" %% "cats-core" % "0.8.1",
    libraryDependencies += "com.ideal.linked" %% "toposoid-knowledgebase-model" % "0.1.0",
    libraryDependencies += "com.ideal.linked" %% "scala-common" % "0.1.0",
    libraryDependencies += "com.ideal.linked" %% "toposoid-common" % "0.1.1",
    libraryDependencies += "io.jvm.uuid" %% "scala-uuid" % "0.3.1",
    libraryDependencies += "com.typesafe.play" %% "play" % "2.8.8",
    libraryDependencies += scalaTest % Test
  )
  .enablePlugins(AutomateHeaderPlugin)

organizationName := "Linked Ideal LLC.[https://linked-ideal.com/]"
startYear := Some(2021)
licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))

