import Dependencies._
import de.heikoseeberger.sbtheader.License

ThisBuild / scalaVersion     := "3.3.6"
ThisBuild / version          := "0.7-SNAPSHOT"
ThisBuild / organization     := "com.ideal.linked"

//addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)

lazy val root = (project in file("."))
  .settings(
    name := "toposoid-sentence-parser-japanese",
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq(
      "com.enjapan" %% "scala-juman-knp" % "0.0.13-SNAPSHOT",
      "org.typelevel" %% "cats-core" % "2.9.0"
    ),
    dependencyOverrides += "org.typelevel" %% "cats-core" % "2.9.0" exclude("org.slf4j","slf4j-api"),
    libraryDependencies += "com.ideal.linked" %% "toposoid-knowledgebase-model" % "0.7-SNAPSHOT" exclude("org.slf4j","slf4j-api"),
    libraryDependencies += "com.ideal.linked" %% "toposoid-deduction-protocol-model" % "0.7-SNAPSHOT" exclude("org.slf4j","slf4j-api"),
    libraryDependencies += "com.ideal.linked" %% "scala-common" % "0.7-SNAPSHOT" exclude("org.slf4j","slf4j-api"),
    libraryDependencies += "com.ideal.linked" %% "toposoid-common" % "0.7-SNAPSHOT" exclude("org.slf4j","slf4j-api"),
    libraryDependencies += "org.playframework" %% "play" % "3.0.7" exclude("org.slf4j","slf4j-api"),
    libraryDependencies += "com.ibm.icu" % "icu4j" % "63.1" exclude("org.slf4j","slf4j-api"),
    libraryDependencies += "commons-lang" % "commons-lang" % "2.6" exclude("org.slf4j","slf4j-api"),
    libraryDependencies += scalaTest % Test exclude("org.slf4j","slf4j-api"),
    libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.36"
  )
  .enablePlugins(AutomateHeaderPlugin)

organizationName := "Linked Ideal LLC.[https://linked-ideal.com/]"
startYear := Some(2021)
licenses += ("AGPL-3.0-or-later", url("http://www.gnu.org/licenses/agpl-3.0.en.html"))
headerLicense := Some(License.AGPLv3("2025", organizationName.value))

