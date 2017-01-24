import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import org.scalajs.sbtplugin.cross.CrossProject
import sbt.Keys._
import sbt._
import sbt.complete.Parser
import scoverage.ScoverageKeys.coverageExcludedPackages

object SlateBuild {

  import ChromeBuild._

  val repl: TaskKey[Unit] = TaskKey[Unit]("repl")

  val unitTest: TaskKey[Unit] = TaskKey[Unit]("unitTest")
  val itTest: TaskKey[Unit] = TaskKey[Unit]("itTest")
  val nonBrowserTest: TaskKey[Unit] = TaskKey[Unit]("nonBrowserTest")
  val stackTest: TaskKey[Unit] = TaskKey[Unit]("stackTest")
  //val unitTestQuick = TaskKey[Unit]("unitTestQuick")
  //val itTestQuick = TaskKey[Unit]("itTestQuick")

  val baseSettings: Seq[sbt.Def.Setting[_]] = Seq(
    version := "0.0.4",
    scalaVersion := "2.11.8",
    updateOptions ~= (_.withCachedResolution(true)),
    scalacOptions ++= Seq(
      "-encoding", "UTF-8",
      "-Xlint",
      "-Xexperimental",
      "-deprecation",
      "-feature",
      "-language:implicitConversions",
      "-language:higherKinds",
      "-unchecked",
      "-Xfatal-warnings",
      "-Yno-adapted-args",
      "-Ywarn-unused-import",
      "-Ywarn-adapted-args",
      "-Ywarn-inaccessible",
      "-Ywarn-infer-any",
      "-Ywarn-nullary-override",
      "-Ywarn-nullary-unit",
      "-Xfuture"
    ),
    scalacOptions in Compile += "-Ywarn-value-discard",
    coverageExcludedPackages := ";qq.*Main;",
    persistLauncher in Compile := true,
    persistLauncher in Test := false,
    unitTest <<= {
      (testOnly in Test).toTask(" -- -oD -l WebTest -l StackTest")
    },
    nonBrowserTest <<= {
      (testOnly in Test).toTask(" -- -oD -l WebTest -n StackTest")
    },
    itTest <<= {
      (testOnly in Test).toTask(" -- -oD -n WebTest -n StackTest")
    },
    stackTest <<= {
      (testOnly in Test).toTask(" -- -oD -n StackTest")
    },
    addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full),
    addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.3" cross CrossVersion.binary)
  )

  val jsSettings: Seq[sbt.Def.Setting[_]] = Seq(
    (relativeSourceMaps in fullOptJS) := true,
    (relativeSourceMaps in fastOptJS) := true
  )

  // amazingly hard to do
  def emptyInputTask: Def.Initialize[InputTask[Unit]] =
    InputTask.createDyn[String, Unit](
      InputTask.parserAsInput(
        Parser.zeroOrMore(
          Parser.charClass(_ => true)).map(_.mkString))
    )(Def.task { (_: String) => Def.task(()) })

  private val disableTests: Seq[Def.Setting[_]] = Seq(
    test in Test := (),
    testQuick in Test <<= emptyInputTask,
    testOnly in Test <<= emptyInputTask
  )

  def dependOnChrome[T](chromeKey: TaskKey[sbt.File], taskKey: TaskKey[T]): Def.Setting[Task[T]] =
    taskKey <<= taskKey.dependsOn(chromeKey in ui)

  lazy val qq: CrossProject = crossProject.in(file("qq"))
    .settings(baseSettings)
    .settings(Dependencies.commonDeps)
    .jsSettings(jsSettings: _*)
    .jsSettings(ScalaJSPlugin.projectSettings: _*)

  lazy val qqjvm: Project = qq.jvm
  lazy val qqjs: Project = qq.js

  lazy val qqmacros: CrossProject = crossProject.in(file("qqmacros"))
    .dependsOn(qq)
    .settings(baseSettings)
    .settings(Dependencies.commonDeps)
    .settings(Dependencies.scalaCompiler)
    .jsSettings(jsSettings)
    .jsSettings(ScalaJSPlugin.projectSettings)

  lazy val qqmacrosjvm: Project = qqmacros.jvm
  lazy val qqmacrosjs: Project = qqmacros.js

  lazy val ui: Project = project.in(file("ui"))
    .dependsOn(qqjs)
    .dependsOn(qqmacrosjs)
    .settings(ScalaJSPlugin.projectSettings)
    .enablePlugins(ScalaJSPlugin)
    .settings(baseSettings)
    .settings(chromeTasks)
    .settings(jsSettings)
    .settings(Dependencies.commonDeps)
    .settings(Dependencies.uiDeps)

  lazy val uitests: Project = project.in(file("uitests"))
    .dependsOn(ui)
    .dependsOn(qqjvm)
    .settings(libraryDependencies += Dependencies.selenium)
    .settings(baseSettings)
    .settings(Dependencies.commonDeps)
    .settings(dependOnChrome(unpackedProd, testOptions in Test))
    .settings((test in Test) <<= (test in Test).dependsOn(compile in Test in qqjvm))
    .settings((testOptions in Test) <<= (testOptions in Test).dependsOn(compile in Test in qqjvm))
    .settings((testQuick in Test) := (test in Test).value)
    .settings(unitTest := ())
    .settings(nonBrowserTest := ())
    .settings(stackTest := ())

  lazy val uibench: Project = project.in(file("uibench"))
    .dependsOn(ui)
    .dependsOn(qqjs)
    .settings(ScalaJSPlugin.projectSettings)
    .enablePlugins(ScalaJSPlugin)
    // same resources as in ui
    .settings((resourceDirectory in Compile) := (resourceDirectory in Compile in ui).value)
    .settings(baseSettings)
    .settings(disableTests)
    .settings(chromeTasks)
    .settings(jsSettings)
    .settings(Dependencies.commonDeps)
    .settings(Dependencies.uiDeps)
    .settings(libraryDependencies += "com.github.japgolly.scalajs-benchmark" %%% "benchmark" % "0.2.4-SNAPSHOT")
    // otherwise scalajs-benchmark doesn't work
    .settings(jsManifestFilter := {
    import org.scalajs.core.tools.jsdep.{JSDependency, JSDependencyManifest}

    (seq: Traversable[JSDependencyManifest]) => {
      seq map { manifest =>

        def isOkToInclude(jsDep: JSDependency): Boolean = {
          !List("react-dom", "react-with-addons").exists(jsDep.resourceName.startsWith)
        }

        new JSDependencyManifest(
          origin = manifest.origin,
          libDeps = manifest.libDeps filter isOkToInclude,
          requiresDOM = manifest.requiresDOM,
          compliantSemantics = manifest.compliantSemantics
        )
      }
    }
  })

  lazy val slate: Project = project.in(file("."))
    .aggregate(ui, uitests, qqjvm, qqjs, qqmacrosjvm, qqmacrosjs)
    .settings(Defaults.projectCore)
    .settings(baseSettings)
    .settings(ScalaJSPlugin.globalSettings)
    .settings(disableTests)

}
