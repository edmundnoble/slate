import sbt._
import Keys._
import net.lullabyte.Chrome
import org.scalajs.sbtplugin.{OptimizerOptions, ScalaJSPlugin}
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import upickle.Js

object DashboarderBuild extends Build {

  val deps = libraryDependencies ++= Seq(
    "com.lihaoyi" %%% "utest" % "0.4.3" % "test",
    "org.scala-js" %%% "scalajs-dom" % "0.9.0",
    "com.lihaoyi" %%% "upickle" % "0.4.0",
    "com.lihaoyi" %%% "pprint" % "0.4.0",
    "com.github.julien-truffaut" %%% "monocle-core" % "1.2.1",
    "com.github.julien-truffaut" %%% "monocle-generic" % "1.2.1",
    "com.github.julien-truffaut" %%% "monocle-macro" % "1.2.1",
    "com.github.julien-truffaut" %%% "monocle-state" % "1.2.1",
    "com.lihaoyi" %%% "fastparse" % "0.3.7",
    "com.github.marklister" %%% "base64" % "0.2.2",
    "net.lullabyte" %%% "scala-js-chrome" % "0.2.1",
    "com.github.japgolly.scalajs-react" %%% "core" % "0.11.1",
    "com.github.japgolly.scalajs-react" %%% "ext-scalaz72" % "0.11.1",
    "com.github.japgolly.scalajs-react" %%% "ext-monocle" % "0.11.1",
    "com.github.japgolly.scalacss" %%% "ext-react" % "0.4.1",
    "io.monix" %%% "monix" % "2.0-RC3",
    "com.thoughtworks.each" %%% "each" % "0.5.1"
  )

  // React JS itself (Note the filenames, adjust as needed, eg. to remove addons.)
  val jsDeps = jsDependencies ++= Seq(
    "org.webjars.bower" % "react" % "15.0.2"
      / "react-with-addons.js"
      minified "react-with-addons.min.js"
      commonJSName "React",

    "org.webjars.bower" % "react" % "15.0.2"
      / "react-dom.js"
      minified "react-dom.min.js"
      dependsOn "react-with-addons.js"
      commonJSName "ReactDOM",

    "org.webjars.bower" % "react" % "15.0.2"
      / "react-dom-server.js"
      minified "react-dom-server.min.js"
      dependsOn "react-dom.js"
      commonJSName "ReactDOMServer"
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

  object SnakePickle extends upickle.AttributeTagged {
    def camelToSnake(s: String) = {
      val res = s.split("(?=[A-Z])", -1).map(_.toLowerCase).mkString("_")
      res
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
    val content = SnakePickle.write(manifest, indent = 2)
    IO.write(out, content)
    out
  }

  lazy val chromeTasks: Seq[Def.Setting[_]] = Seq(
    chromePackageContent := file("content"),
    chromeBuildOpt := {
      val unpacked = target.value / "chrome" / "unpackedopt"
      (resourceDirectory in Compile).value.listFiles().foreach(f => IO.copyFile(f, unpacked / f.getName))

      Chrome.buildExtentionDirectory(unpacked)(
        (chromeGenerateManifest in Compile).value,
        (fullOptJS in Compile).value.data,
        (packageMinifiedJSDependencies in Compile).value,
        (packageScalaJSLauncher in Compile).value.data,
        (chromePackageContent in Compile).value
      )
    },
    chromeBuildFast := {
      val unpacked = target.value / "chrome" / "unpackedfast"
      (resourceDirectory in Compile).value.listFiles().foreach(f => IO.copyFile(f, unpacked / f.getName))

      Chrome.buildExtentionDirectory(unpacked)(
        (chromeGenerateManifest in Compile).value,
        (fastOptJS in Compile).value.data,
        (packageJSDependencies in Compile).value,
        (packageScalaJSLauncher in Compile).value.data,
        (chromePackageContent in Compile).value
      )
    },
    chromeBuildUnopt := {
      val unpacked = target.value / "chrome" / "unpackedunopt"
      (resourceDirectory in Compile).value.listFiles().foreach(f => IO.copyFile(f, unpacked / f.getName))
      val oldOpts = scalaJSOptimizerOptions.value
      scalaJSOptimizerOptions := oldOpts.withDisableOptimizer(true)
      val file =
        Chrome.buildExtentionDirectory(unpacked)(
          (chromeGenerateManifest in Compile).value,
          (fastOptJS in Compile).value.data,
          (packageJSDependencies in Compile).value,
          (packageScalaJSLauncher in Compile).value.data,
          (chromePackageContent in Compile).value
        )
      scalaJSOptimizerOptions := oldOpts
      file
    },
    chromePackage := {
      val out = target.value / "chrome"
      val chromeAppDir = chromeBuildOpt.value
      val zipFile = new File(out, s"${name.value}.zip")
      IO.zip(allSubpaths(chromeAppDir), zipFile)
      zipFile
    },
    chromeGenerateManifest := {
      generateManifest(target.value / "chrome" / "generated_manifest.json")(ChromeManifest.mySettings)
    }
  )

  lazy val replMain =
    mainClass in(Compile, run) := Some("edmin.qq.InterpreterMain")

  val otherSettings: Seq[sbt.Def.Setting[_]] = Seq(
    version := "0.0.1",
    scalaVersion := "2.11.8",
    persistLauncher in Compile := false,
    persistLauncher in Test := false,
    testFrameworks += new TestFramework("utest.runner.Framework"),
    addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)
  )

  val jsSettings: Seq[sbt.Def.Setting[Boolean]] = Seq(
    scalaJSUseRhino in Global := false,
    relativeSourceMaps := true
  )

  val sets: Seq[Def.Setting[_]] = Defaults.projectCore ++ otherSettings ++ deps

  lazy val root: Project = Project(id = "root", base = file("."))
    .aggregate(qq, ui)
    .settings(ScalaJSPlugin.globalSettings)
    .settings(scalaVersion := "2.11.8")
    .settings(name := "dashboarder")

  lazy val qq: Project = Project(id = "qq", base = file("qq"))
    .settings(ScalaJSPlugin.projectSettings)
    .enablePlugins(ScalaJSPlugin)
    .settings(jsSettings)
    .settings(sets)
    .settings(name := "dashboarder")
    .settings(replMain)

  lazy val ui = Project(id = "ui", base = file("ui"))
    .dependsOn(qq)
    .settings(ScalaJSPlugin.projectSettings)
    .enablePlugins(ScalaJSPlugin)
    .settings(chromeTasks)
    .settings(jsSettings)
    .settings(sets)
    .settings(name := "ui")

}
