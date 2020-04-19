enablePlugins(ScalaJSPlugin)

name := "FermiPicoBagel"
scalaVersion := "2.13.1"

scalaJSUseMainModuleInitializer := true
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.0.0"
scalacOptions ++= Seq("-unchecked", "-deprecation")