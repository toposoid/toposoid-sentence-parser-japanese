import Dependencies._

ThisBuild / scalaVersion     := "2.13.11"
ThisBuild / version          := "0.5-SNAPSHOT"
ThisBuild / organization     := "com.ideal.linked"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)

lazy val root = (project in file("."))
  .settings(
    name := "toposoid-sentence-parser-japanese",
    libraryDependencies ++= Seq(
      "com.enjapan" %% "scala-juman-knp" % "0.0.11-SNAPSHOT",
      "org.typelevel" %% "cats-core" % "2.9.0"
    ),
    dependencyOverrides += "org.typelevel" %% "cats-core" % "2.9.0",
    libraryDependencies += "com.ideal.linked" %% "toposoid-knowledgebase-model" % "0.5-SNAPSHOT",
    libraryDependencies += "com.ideal.linked" %% "toposoid-deduction-protocol-model" % "0.5-SNAPSHOT",
    libraryDependencies += "com.ideal.linked" %% "scala-common" % "0.5-SNAPSHOT",
    libraryDependencies += "com.ideal.linked" %% "toposoid-common" % "0.5-SNAPSHOT",
    libraryDependencies += "io.jvm.uuid" %% "scala-uuid" % "0.3.1",
    libraryDependencies += "com.typesafe.play" %% "play" % "2.8.8",
    libraryDependencies += "com.ibm.icu" % "icu4j" % "63.1",
    libraryDependencies += "commons-lang" % "commons-lang" % "2.6",
    libraryDependencies += scalaTest % Test
  )
  .enablePlugins(AutomateHeaderPlugin)

organizationName := "Linked Ideal LLC.[https://linked-ideal.com/]"
startYear := Some(2021)
licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt"))

