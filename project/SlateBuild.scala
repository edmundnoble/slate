import chrome.Manifest
import net.lullabyte.Chrome
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import org.scalajs.sbtplugin.cross.CrossProject
import org.scalajs.sbtplugin.{AbstractJSDep, ScalaJSPlugin}
import sbt.Keys._
import sbt._
import sbt.complete.Parser
import scoverage.ScoverageKeys.coverageExcludedPackages
import upickle.Js

object SlateBuild {

  lazy val commonDeps: Seq[Setting[_]] = Seq(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "0.8.1",
      "org.typelevel" %%% "cats-kernel" % "0.8.1",
      "org.typelevel" %%% "cats-macros" % "0.8.1",
      "org.typelevel" %%% "cats-free" % "0.8.1",
      "org.atnos" %%% "eff-cats" % "2.0.0-RC26",
      "org.atnos" %%% "eff-cats-monix" % "2.0.0-RC26",
      "org.scalatest" %%% "scalatest" % "3.0.1" % "test",
      "com.lihaoyi" %%% "upickle" % "0.4.3",
      "com.lihaoyi" %%% "fastparse" % "0.4.1",
      "io.monix" %%% "monix-types" % "2.1.1",
      "io.monix" %%% "monix-eval" % "2.1.1",
      "io.monix" %%% "monix-execution" % "2.1.1",
      "io.monix" %%% "monix-reactive" % "2.1.1",
      "io.monix" %%% "monix-cats" % "2.1.1",
      "org.scodec" %%% "scodec-bits" % "1.1.2",
      "org.scodec" %%% "scodec-core" % "1.10.3",
      "com.chuusai" %%% "shapeless" % "2.3.2"
    ),
    resolvers += Resolver.sonatypeRepo("releases"),
    resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/"
  )

  lazy val uiDeps: Setting[Seq[ModuleID]] = libraryDependencies ++= Seq(
    "net.lullabyte" %%% "scala-js-chrome" % "0.2.1",
    "org.scala-js" %%% "scalajs-dom" % "0.9.0",
    "com.github.japgolly.scalajs-react" %%% "core" % "0.11.3",
    "com.github.japgolly.scalajs-react" %%% "extra" % "0.11.3",
    "com.github.japgolly.scalacss" %%% "core" % "0.5.0",
    "com.github.japgolly.scalacss" %%% "ext-react" % "0.5.0"
  )

  //   React JS itself (Note the filenames, adjust as needed, eg. to remove addons.)
  lazy val jsDeps: Setting[Seq[AbstractJSDep]] = jsDependencies ++= Seq(
    "org.webjars.bower" % "react" % "15.0.1"
      / "react-with-addons.js"
      minified "react-with-addons.min.js"
      commonJSName "React",

    "org.webjars.bower" % "react" % "15.0.1"
      / "react-dom.js"
      minified "react-dom.min.js"
      dependsOn "react-with-addons.js"
      commonJSName "ReactDOM",

    "org.webjars" % "chartjs" % "1.0.2"
      / "Chart.js"
      minified "Chart.min.js"
  )

  val chromePackageContent: SettingKey[File] = SettingKey[File]("chromePackageContent",
    "The contents of this directory get copied to the into the chrome extension")
  val unpackedProd: TaskKey[File] = TaskKey[File]("unpackedProd")
  val unpackedDevFast: TaskKey[File] = TaskKey[File]("unpackedDevFast")
  val unpackedDevOpt: TaskKey[File] = TaskKey[File]("unpackedDevOpt")
  val chromePackage: TaskKey[File] = TaskKey[File]("chromePackage")
  val chromeGenerateManifest: TaskKey[File] = TaskKey[File]("chromeGenerateManifest")
  val chromeManifest: TaskKey[Manifest] = TaskKey[chrome.Manifest]("chromeManifest")
  val repl: TaskKey[Unit] = TaskKey[Unit]("repl")

  object SnakeOptionPickle extends upickle.AttributeTagged {
    def camelToSnake(s: String): String = {
      s.split("(?=[A-Z])", -1).map(_.toLowerCase).mkString("_")
    }
    override def CaseR[T: this.Reader, V]
    (f: T => V,
     names: Array[String],
     defaults: Array[Js.Value]): SnakeOptionPickle.Reader[V] = {
      super.CaseR[T, V](f, names.map(camelToSnake), defaults)
    }
    override def CaseW[T: this.Writer, V]
    (f: V => Option[T],
     names: Array[String],
     defaults: Array[Js.Value]): SnakeOptionPickle.Writer[V] = {
      super.CaseW[T, V](f, names.map(camelToSnake), defaults)
    }
    override implicit def OptionW[T: Writer]: Writer[Option[T]] = Writer {
      case None => Js.Null
      case Some(s) => implicitly[Writer[T]].write(s)
    }
    override implicit def OptionR[T: Reader]: Reader[Option[T]] = Reader {
      case Js.Null => None
      case v: Js.Value => Some(implicitly[Reader[T]].read.apply(v))
    }
  }

  def generateManifest(out: File)(manifest: ChromeManifest): File = {
    val content = SnakeOptionPickle.write(manifest, indent = 2)
    IO.write(out, content)
    out
  }

  def copyResources(sourceMap: sbt.File, suffices: Seq[String]): Def.Initialize[Task[File]] = Def.task {
    val file = (resourceDirectory in Compile).value
    val targetValue = target.value
    val unpacked = suffices.foldLeft(targetValue)(_ / _)
    file.listFiles().foreach { resourceFile =>
      val target = unpacked / resourceFile.getName
      if (!target.exists() || resourceFile.lastModified() > target.lastModified()) {
        if (resourceFile.isFile) {
          IO.copyFile(resourceFile, target)
        } else {
          IO.copyDirectory(resourceFile, target)
        }
      }
    }
    IO.copyFile(sourceMap, unpacked / "main.js.map")
    unpacked
  }

  def defineChromeBuildTask(folderName: String, buildTaskKey: TaskKey[Attributed[sbt.File]]): Def.Initialize[Task[File]] = Def.taskDyn {
    val r = (buildTaskKey in Compile).value
    val unpacked = Def.task {
      copyResources(r.get(scalaJSSourceMap).get, Seq("chrome", folderName)).value
    }
    Def.task {
      val file =
        Chrome.buildExtentionDirectory(unpacked.value)(
          (chromeGenerateManifest in Compile).value,
          r.data,
          (packageMinifiedJSDependencies in Compile).value,
          (packageScalaJSLauncher in Compile).value.data,
          (chromePackageContent in Compile).value
        )
      file
    }
  }

  lazy val chromeTasks: Seq[Def.Setting[_]] = Seq(
    chromePackageContent := file("content"),

    unpackedProd <<= Def.task {
      scalaJSSemantics ~= (_.withProductionMode(true))
      defineChromeBuildTask("unpackedopt", fullOptJS).value
    },

    unpackedDevFast <<= Def.task {
      defineChromeBuildTask("unpackedfast", fastOptJS).value
    },

    unpackedDevOpt <<= Def.task {
      val oldOpts = scalaJSOptimizerOptions.value
      scalaJSOptimizerOptions := oldOpts.withPrettyPrintFullOptJS(true)
      val file = defineChromeBuildTask("unpackedunopt", fastOptJS).value
      //      scalaJSOptimizerOptions := oldOpts
      file
    },

    chromePackage <<= Def.task {
      val out = target.value / "chrome"
      val chromeAppDir = unpackedProd.value
      val zipFile = new File(out, name.value + ".zip")
      IO.zip(allSubpaths(chromeAppDir), zipFile)
      zipFile
    },

    chromeGenerateManifest <<= Def.task {
      generateManifest(target.value / "chrome" / "generated_manifest.json")(ChromeManifest.mySettings)
    }

  )

  val unitTest: TaskKey[Unit] = TaskKey[Unit]("unitTest")
  val itTest: TaskKey[Unit] = TaskKey[Unit]("itTest")
  val nonBrowserTest: TaskKey[Unit] = TaskKey[Unit]("nonBrowserTest")
  val stackTest: TaskKey[Unit] = TaskKey[Unit]("stackTest")
  //val unitTestQuick = TaskKey[Unit]("unitTestQuick")
  //val itTestQuick = TaskKey[Unit]("itTestQuick")

  val baseSettings: Seq[sbt.Def.Setting[_]] = Seq(
    version := "0.0.1",
    scalaVersion := "2.11.8",
    updateOptions ~= (_.withConsolidatedResolution(true)),
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
    scalaJSUseRhino in Global := false,
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
    .settings(baseSettings: _*)
    .settings(commonDeps: _*)
    .jsSettings(jsSettings: _*)
    .jsSettings(ScalaJSPlugin.projectSettings: _*)

  lazy val qqjvm: Project = qq.jvm
  lazy val qqjs: Project = qq.js

  lazy val qqmacros: CrossProject = crossProject.in(file("qqmacros"))
    .dependsOn(qq)
    .settings(baseSettings: _*)
    .settings(commonDeps: _*)
    .settings(libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _))
    .jsSettings(jsSettings: _*)
    .jsSettings(ScalaJSPlugin.projectSettings: _*)

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
    .settings(commonDeps)
    .settings(uiDeps)

  lazy val uitests: Project = project.in(file("uitests"))
    .dependsOn(ui)
    .dependsOn(qqjvm)
    .settings(libraryDependencies += "org.seleniumhq.selenium" % "selenium-java" % "2.35.0" % "test")
    .settings(baseSettings)
    .settings(commonDeps)
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
    .settings((resourceDirectory in Compile) := (resourceDirectory in Compile in ui).value)
    .settings(baseSettings: _*)
    .settings(disableTests: _*)
    .settings(chromeTasks)
    .settings(jsSettings)
    .settings(commonDeps: _*)
    .settings(uiDeps: _*)
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
    .aggregate(ui, uitests, uibench, qqjvm, qqjs, qqmacrosjvm, qqmacrosjs)
    .settings(Defaults.projectCore)
    .settings(baseSettings)
    .settings(ScalaJSPlugin.globalSettings)
    .settings(disableTests)

}
