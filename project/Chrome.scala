import chrome.Manifest
import net.lullabyte.Chrome
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbt.Keys._
import sbt._
import upickle.Js

object ChromeBuild {

  val chromePackageContent: SettingKey[File] = SettingKey[File]("chromePackageContent",
    "The contents of this directory get copied to the into the chrome extension")
  val unpackedProd: TaskKey[File] = TaskKey[File]("unpackedProd")
  val unpackedDevFast: TaskKey[File] = TaskKey[File]("unpackedDevFast")
  val unpackedDevOpt: TaskKey[File] = TaskKey[File]("unpackedDevOpt")
  val chromePackage: TaskKey[File] = TaskKey[File]("chromePackage")
  val chromeGenerateManifest: TaskKey[File] = TaskKey[File]("chromeGenerateManifest")
  val chromeManifest: TaskKey[Manifest] = TaskKey[chrome.Manifest]("chromeManifest")

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
      generateManifest(target.value / "chrome" / "generated_manifest.json")(ChromeManifest.mySettings(version.value))
    }

  )

}