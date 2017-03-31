import org.scalajs.sbtplugin.AbstractJSDep
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbt.Keys._
import sbt._

object Dependencies {

  lazy val commonDeps: Seq[Setting[_]] = Seq(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "0.9.0",
      "org.typelevel" %%% "cats-kernel" % "0.9.0",
      "org.typelevel" %%% "cats-macros" % "0.9.0",
      "org.typelevel" %%% "cats-free" % "0.9.0",
      "org.atnos" %%% "eff" % "3.1.0",
      "org.atnos" %%% "eff-monix" % "3.1.0",
      "org.scalatest" %%% "scalatest" % "3.0.1" % "test",
      "com.lihaoyi" %%% "upickle" % "0.4.4",
      "com.lihaoyi" %%% "fastparse" % "0.4.2",
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
    "net.lullabyte" %%% "scala-js-chrome" % "0.4.0",
    "org.scala-js" %%% "scalajs-dom" % "0.9.1",
    "com.github.japgolly.scalajs-react" %%% "core" % "0.11.3",
    "com.github.japgolly.scalajs-react" %%% "extra" % "0.11.3",
    "com.github.japgolly.scalacss" %%% "core" % "0.5.1",
    "com.github.japgolly.scalacss" %%% "ext-react" % "0.5.1"
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

  lazy val selenium =
    "org.seleniumhq.selenium" % "selenium-java" % "2.35.0" % "test"

  lazy val scalaCompiler =
    libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _)

}
