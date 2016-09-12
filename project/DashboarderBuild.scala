import net.lullabyte.Chrome
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbt._, Keys._
import sbt.complete.Parser
import scoverage.ScoverageKeys.coverageExcludedPackages
import upickle.Js

object DashboarderBuild {

  lazy val commonDeps = Seq(
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.0.0" % "test",
      "com.lihaoyi" %%% "upickle" % "0.4.1",
      "com.lihaoyi" %%% "fastparse" % "0.3.7",
      "io.monix" %%% "monix" % "2.0-RC13",
      "io.monix" %%% "monix-scalaz-72" % "2.0-RC13",
      "com.thoughtworks.each" %%% "each" % "0.5.1",
      "com.slamdata" %%% "matryoshka-core" % "0.11.0",
      "org.scodec" %%% "scodec-bits" % "1.1.0",
      "org.scodec" %%% "scodec-core" % "1.10.2"
    ),
    resolvers += Resolver.sonatypeRepo("releases"),
    resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/")

  lazy val uiDeps = libraryDependencies ++= Seq(
    "net.lullabyte" %%% "scala-js-chrome" % "0.2.1",
    "org.scala-js" %%% "scalajs-dom" % "0.9.0",
    "com.github.japgolly.scalajs-react" %%% "core" % "0.11.1",
    "com.github.japgolly.scalajs-react" %%% "ext-scalaz72" % "0.11.1",
    "com.github.japgolly.scalajs-react" %%% "ext-monocle" % "0.11.1",
    "com.github.japgolly.scalacss" %%% "ext-react" % "0.5.0"
  )

  //   React JS itself (Note the filenames, adjust as needed, eg. to remove addons.)
  lazy val jsDeps = jsDependencies ++= Seq(
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

  val chromePackageContent = SettingKey[File]("chromePackageContent",
    "The contents of this directory get copied to the into the chrome extension")
  val chromeBuildOpt = TaskKey[File]("chromeBuildOpt")
  val chromeBuildFast = TaskKey[File]("chromeBuildFast")
  val chromeBuildUnopt = TaskKey[File]("chromeBuildUnopt")
  val chromePackage = TaskKey[File]("chromePackage")
  val chromeGenerateManifest = TaskKey[File]("chromeGenerateManifest")
  val chromeManifest = TaskKey[chrome.Manifest]("chromeManifest")
  val repl = TaskKey[Unit]("repl")

  object SnakeOptionPickle extends upickle.AttributeTagged {
    def camelToSnake(s: String) = {
      s.split("(?=[A-Z])", -1).map(_.toLowerCase).mkString("_")
    }
    override def CaseR[T: this.Reader, V]
    (f: T => V,
     names: Array[String],
     defaults: Array[Js.Value]) = {
      super.CaseR[T, V](f, names.map(camelToSnake), defaults)
    }
    override def CaseW[T: this.Writer, V]
    (f: V => Option[T],
     names: Array[String],
     defaults: Array[Js.Value]) = {
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

  def copyResources(sourceMap: sbt.File, suffices: Seq[String]) = Def.task {
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

  def defineChromeBuildTask(folderName: String, buildTaskKey: TaskKey[Attributed[sbt.File]]) = Def.taskDyn {
    val r = (buildTaskKey in Compile).value
    val unpacked = Def.task { copyResources(r.get(scalaJSSourceMap).get, Seq("chrome", folderName)).value }
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

    chromeBuildOpt := {
      defineChromeBuildTask("unpackedopt", fullOptJS).value
    },

    chromeBuildFast := {
      defineChromeBuildTask("unpackedfast", fastOptJS).value
    },

    chromeBuildUnopt := {
      val oldOpts = scalaJSOptimizerOptions.value
      scalaJSOptimizerOptions := oldOpts.withDisableOptimizer(true)
      val file = defineChromeBuildTask("unpackedunopt", fastOptJS).value
      scalaJSOptimizerOptions := oldOpts
      file
    },

    chromePackage := {
      val out = target.value / "chrome"
      val chromeAppDir = chromeBuildOpt.value
      val zipFile = new File(out, name.value + ".zip")
      IO.zip(allSubpaths(chromeAppDir), zipFile)
      zipFile
    },

    chromeGenerateManifest := {
      generateManifest(target.value / "chrome" / "generated_manifest.json")(ChromeManifest.mySettings)
    }

  )

  lazy val replMain =
    mainClass in(Compile, run) := Some("qq.InterpreterMain")

  val unitTest = TaskKey[Unit]("unitTest")
  val itTest = TaskKey[Unit]("itTest")
  val stackTest = TaskKey[Unit]("stackTest")
  //val unitTestQuick = TaskKey[Unit]("unitTestQuick")
  //val itTestQuick = TaskKey[Unit]("itTestQuick")

  val baseSettings: Seq[sbt.Def.Setting[_]] = Seq(
    version := "0.0.1",
    scalaVersion := "2.11.8",
    scalacOptions ++= Seq(
      "-Xlint",
      "-Xexperimental",
      "-deprecation",
      "-feature",
      "-language:higherKinds",
      "-Ywarn-adapted-args",
      "-Ywarn-adapted-args",
      "-Ywarn-inaccessible",
      "-Ywarn-infer-any",
      "-Ywarn-nullary-override",
      "-Ywarn-nullary-unit"
    ),
    scalacOptions in Compile += "-Ywarn-value-discard",
    coverageExcludedPackages := ";qq.*Main;",
    persistLauncher in Compile := true,
    persistLauncher in Test := false,
    unitTest <<= {
      (testOnly in Test).toTask(" -- -oD -l WebTest -l StackTest")
    },
    itTest <<= {
      (testOnly in Test).toTask(" -- -oD -n WebTest -n StackTest")
    },
    stackTest <<= {
      (testOnly in Test).toTask(" -- -oD -n StackTest")
    },
    addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full),
    addCompilerPlugin("org.spire-math" % "kind-projector" % "0.8.0" cross CrossVersion.binary)
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

  lazy val qq = crossProject.in(file("qq"))
    .settings(baseSettings: _*)
    .settings(commonDeps: _*)
    .settings(replMain: _*)
    .jsSettings(scalaJSUseRhino in Global := false)
    .jsSettings(ScalaJSPlugin.projectSettings: _*)
    .jsSettings(libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.0")

  lazy val qqjvm = qq.jvm
  lazy val qqjs = qq.js

  lazy val ui = project.in(file("ui"))
    .dependsOn(qqjs)
    .settings(ScalaJSPlugin.projectSettings)
    .enablePlugins(ScalaJSPlugin)
    .settings(baseSettings)
    .settings(chromeTasks)
    .settings(jsSettings)
    .settings(commonDeps)
    .settings(uiDeps)
    .settings(disableTests)

  lazy val uitests = project.in(file("uitests"))
    .dependsOn(ui)
    .dependsOn(qqjvm)
    .settings(libraryDependencies += "org.seleniumhq.selenium" % "selenium-java" % "2.35.0" % "test")
    .settings(baseSettings)
    .settings(commonDeps)
    .settings(dependOnChrome(chromeBuildOpt, testOptions in Test))
    .settings((test in Test) <<= (test in Test).dependsOn(compile in Test in qqjvm))
    .settings((testOptions in Test) <<= (testOptions in Test).dependsOn(compile in Test in qqjvm))
    .settings((testQuick in Test) := (test in Test).value)
    .settings(unitTest := ())
    .settings(stackTest := ())

  lazy val uibench = project.in(file("uibench"))
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
    .settings(libraryDependencies += "com.github.japgolly.scalajs-benchmark" %%% "benchmark" % "0.2.3")
    // otherwise scalajs-benchmark doesn't work
    .settings(jsManifestFilter := {
      import org.scalajs.core.tools.jsdep.{JSDependencyManifest, JSDependency}

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

  lazy val root = project.in(file("."))
    .aggregate(ui, uitests, uibench, qqjvm, qqjs)
    .settings(Defaults.projectCore)
    .settings(baseSettings)
    .settings(ScalaJSPlugin.globalSettings)
    .settings(disableTests)

}
