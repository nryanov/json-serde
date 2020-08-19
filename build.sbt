resolvers += "Sonatype OSS Snapshots".at("https://oss.sonatype.org/content/repositories/snapshots")

addCompilerPlugin(
  ("org.typelevel" %% "kind-projector" % "0.11.0").cross(CrossVersion.full)
)

lazy val Bench = config("bench").extend(Test)

lazy val root = (project in file("."))
  .configs(Bench)
  .enablePlugins(Antlr4Plugin)
  .settings(inConfig(Bench)(Defaults.testSettings): _*)
  .settings(
    name := "json-serde",
    scalaVersion := "2.13.3",
    Antlr4 / antlr4Version := "4.7.2",
    Antlr4 / antlr4PackageName := Some("jsonserde.antlr"),
    Antlr4 / antlr4GenListener := false,
    Antlr4 / antlr4GenVisitor := true,
    Antlr4 / antlr4TreatWarningsAsErrors := true,
    Antlr4 / javaSource := (sourceManaged in Compile).value,
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.3",
      "org.scalatest" %% "scalatest" % "3.1.0" % "test,bench",
      "com.storm-enroute" %% "scalameter" % "0.19" % "bench",
      "io.circe" %% "circe-core" % "0.13.0" % "bench",
      "io.circe" %% "circe-parser" % "0.13.0" % "bench",
      "io.circe" %% "circe-generic" % "0.13.0" % "bench"
    ),
    scalacOptions := Seq(
      //  "-Xlog-implicits",
      "-encoding",
      "utf8",
      "-Xfatal-warnings",
      "-deprecation",
      "-unchecked",
      "-language:implicitConversions",
      "-language:higherKinds",
      "-language:existentials",
      "-language:postfixOps"
    ),
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    parallelExecution := false,
    logBuffered := false
  )
