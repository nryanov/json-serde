name := "json-serde"

scalaVersion := "2.13.3"

addCompilerPlugin(
  ("org.typelevel" %% "kind-projector" % "0.11.0").cross(CrossVersion.full)
)

enablePlugins(Antlr4Plugin)

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
)

// Antlr4 settings
antlr4Version in Antlr4 := "4.7.2"
antlr4PackageName in Antlr4 := Some("jsonserde.antlr")
antlr4GenListener in Antlr4 := true
antlr4GenVisitor in Antlr4 := true
antlr4TreatWarningsAsErrors in Antlr4 := true
javaSource in Antlr4 := (sourceManaged in Compile).value

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.scalatest" %% "scalatest" % "3.1.0" % Test
)
