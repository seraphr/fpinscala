import scalariform.formatter.preferences._

val Organization = "jp.seraphr"
val ScalaVersion = "2.11.6"
val Version = "0.1.0-SNAPSHOT"

val RiformSettings = scalariformSettings ++ (
  ScalariformKeys.preferences := (ScalariformKeys.preferences.value
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(AlignSingleLineCaseStatements.MaxArrowIndent, 60))
    .setPreference(DoubleIndentClassDeclaration, false))

val CommonDependencies = Seq(
  "org.scalatest" %% "scalatest" % "2.2.5" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.1" % "test"
)

val CommonSettings = Seq(
  incOptions := incOptions.value.withNameHashing(true),
  updateOptions := updateOptions.value.withCachedResolution(true),
  organization := Organization,
  scalaVersion := ScalaVersion,
  version := Version,
  testOptions in Test := Seq(
    Tests.Argument("-oS", "-u", "target/junit"),
    Tests.Argument("-l", "org.scalatest.tags.Slow")
  ),
  scalacOptions in (Compile, doc) ++= Seq("-groups", "-implicits", "-diagrams"),
  scalacOptions ++= Seq("-encoding", "UTF-8", "-feature", "-deprecation", "-Xlint"),
  javacOptions ++= Seq("-encoding", "UTF-8")
) ++ RiformSettings

// root project
lazy val root = Project(
  id = "root",
  base = file("."),
  settings = CommonSettings ++ Seq(
    TaskKey[Unit]("checkScalariform") := {
      val diff = "git diff".!!
      if(diff.nonEmpty){
        sys.error("Working directory is dirty!\n" + diff)
      }
    },
    publish := { }
  )
) aggregate (
  subProject
)

// sub projects

lazy val subProject = {
  val subProjectName = "fp-in-scala"
  Project(
    id = subProjectName,
    base = file(s"./${subProjectName}"),
    settings = CommonSettings ++ Seq(
      name := subProjectName,
      libraryDependencies ++= CommonDependencies
    )
  )
}

