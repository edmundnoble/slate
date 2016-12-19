package slate
package app

import monix.cats._
import cats.implicits._
import japgolly.scalajs.react.{ReactComponentM, ReactDOM, TopNode}
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import monix.reactive.Observable.{fromTask => toObservable}
import org.atnos.eff.syntax.all._
import org.atnos.eff.{Eff, Fx}
import org.scalajs.dom
import qq.Platform.Rec._
import qq.cc.{CompiledFilter, QQCompilationException, QQCompiler, QQRuntimeException}
import qq.data.{FilterAST, JSON, Program}
import shapeless.{:+:, CNil}
import slate.app.builtin.{GCalendarApp, GmailApp}
import slate.app.caching.Caching
import slate.app.caching.Program.ErrorGettingCachedProgram
import slate.app.refresh.BootRefreshPolicy
import slate.models.{AppModel, DatedAppContent, ExpandableContentModel}
import slate.storage.StorageAction._storageAction
import slate.storage.{DomStorage, StorageAction, StorageFS, StorageProgram}
import slate.util.{ExternalVar, LoggerFactory}
import slate.util.Util._
import slate.views.AppView.AppProps
import slate.views.DashboardPage
import slate.views.DashboardPage.SearchPageProps

import scala.concurrent.duration._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.util.Success
import scalacss.defaults.PlatformExports
import scalacss.internal.StringRenderer

@JSExport
object SlateApp extends scalajs.js.JSApp {

  private[this] val logger = LoggerFactory.getLogger("DashboarderApp")

  final class EmptyResponseException extends java.lang.Exception("Empty response")

  type StorageOrTask = Fx.fx2[StorageAction, Task]
  type JustTask = Fx.fx1[Task]

  def builtinPrograms: Vector[SlateProgram[(SlateProgramConfig, Program[FilterAST] Either String)]] =
    Vector(
      SlateProgram(1, "Gmail", "https://gmail.com",
        (SlateProgramConfig(JSON.Obj(), BootRefreshPolicy.IfOlderThan(120)), GmailApp.program)),
      SlateProgram(2, "Google Calendar", "https://calendar.google.com",
        (SlateProgramConfig(JSON.Obj(), BootRefreshPolicy.IfOlderThan(60)), GCalendarApp.program))
    )

  type ErrorCompilingPrograms = QQCompilationException :+: ErrorGettingCachedProgram


  def nonceSource(): String = {
    val int = scala.util.Random.nextInt(1000)
    int.toString
  }

  def prepareProgramFolder[R: _storageAction]: Eff[R, StorageFS.StorageKey[StorageFS.Dir]] = for {
    programDirKey <- StorageFS.mkDir[R]("program", nonceSource, StorageFS.fsroot)
  } yield programDirKey.get.fold(identity[StorageFS.StorageKey[StorageFS.Dir]])

  def compiledPrograms: Task[Vector[SlateProgram[Either[ErrorCompilingPrograms, CompiledFilter]]]] = {

    def getCachedProgramIfApplicable(program: SlateProgram[Either[Program[FilterAST], String]]): Eff[StorageOrTask, SlateProgram[Either[ErrorGettingCachedProgram, Program[FilterAST]]]] = {
      program.program.fold(p =>
        program.withProgram(Either.right[ErrorGettingCachedProgram, Program[FilterAST]](p)).pureEff[StorageOrTask],
        s => caching.Program.getCachedProgramByHash[StorageOrTask](program.withProgram(s)).map(program.withProgram))
    }

    val prog: Eff[StorageOrTask, Vector[SlateProgram[Either[ErrorGettingCachedProgram, Program[FilterAST]]]]] =
      builtinPrograms.map(_.map(_._2)).traverseA(getCachedProgramIfApplicable)

    val makeProgramFolderThenCompilePrograms = for {
      programDirKey <- StorageProgram.runProgramInto(DomStorage.Local, prepareProgramFolder[StorageOrTask])
      cachedPrograms <- StorageFS.runSealedStorageProgramInto(prog, DomStorage.Local, nonceSource, programDirKey)
      cachedCompiledPrograms = cachedPrograms
        .map(_.map(_.leftMap(copSub[ErrorCompilingPrograms](_))
          .flatMap {
            QQCompiler.compileProgram(SlatePrelude, _).leftMap(copInj[ErrorCompilingPrograms][QQCompilationException])
          }
        ))
    } yield cachedCompiledPrograms
    makeProgramFolderThenCompilePrograms.detach
  }

  type ErrorRunningPrograms = QQRuntimeException :+: CNil

  private def runCompiledPrograms(configs: Map[Int, SlateProgramConfig]): Task[Vector[SlateProgram[ErrorCompilingPrograms Either Task[ErrorRunningPrograms Either Vector[JSON]]]]] =
    compiledPrograms.map(_.map(program =>
      program.map(_.map {
        CompiledFilter.run(configs(program.id).input, Map.empty, _).map(
          _.leftMap[ErrorRunningPrograms](exs => inl(QQRuntimeException(exs)))
        )
      })
    ))

  type ErrorDeserializingProgramOutput = upickle.Invalid.Json :+: upickle.Invalid.Data :+: ErrorRunningPrograms

  private def
  deserializeProgramOutput(configs: Map[Int, SlateProgramConfig]): Task[Vector[SlateProgram[ErrorCompilingPrograms Either Task[ErrorDeserializingProgramOutput Either Vector[ExpandableContentModel]]]]] =
    runCompiledPrograms(configs).map {
      _.map(_.map(_.map(_.map(
        _.leftMap(copSub[ErrorDeserializingProgramOutput](_)).flatMap(
          _.traverse[ErrorDeserializingProgramOutput Either ?, ExpandableContentModel] {
            json =>
              val upickleJson = JSON.JSONToUpickleRec(json)
              try Either.right(ExpandableContentModel.pkl.read(upickleJson)) catch {
                case ex: upickle.Invalid.Data => Either.left(copInj[ErrorDeserializingProgramOutput](ex))
              }
          }
        )
      ))))
    }

  def prepareDataFolder: StorageProgram[StorageFS.StorageKey[StorageFS.Dir]] = for {
    dataDirKey <- StorageFS.mkDir("data", nonceSource, StorageFS.fsroot)
  } yield dataDirKey.get.fold(identity[StorageFS.StorageKey[StorageFS.Dir]])

  type AllErrors = upickle.Invalid.Json :+: upickle.Invalid.Data :+: QQRuntimeException :+: ErrorCompilingPrograms
  type UpickleError = upickle.Invalid.Json :+: upickle.Invalid.Data :+: CNil

  def makeDataKey(title: String, input: JSON): String =
    title + "|" + input.hashCode()

  def getCachedOutput(configs: Map[Int, SlateProgramConfig], dataDirKey: StorageFS.StorageKey[StorageFS.Dir], programs: Vector[SlateProgram[Unit]]): Task[Vector[Option[SlateProgram[UpickleError Either DatedAppContent]]]] = {
    for {
      output <-
      StorageFS.runSealedStorageProgramInto[StorageOrTask, JustTask, Task, Vector[Option[SlateProgram[UpickleError Either DatedAppContent]]]](
        programs.traverseA(program =>
          Caching.getCachedBy[StorageOrTask, UpickleError, SlateProgram[Unit], DatedAppContent](program)(
            prg => makeDataKey(prg.title, configs(prg.id).input), {
              encodedModels =>
                for {
                  readJson <-
                  Either.catchOnly[upickle.Invalid.Json](upickle.json.read(encodedModels))
                    .leftMap(copInj[UpickleError](_))
                  readModel <- Either
                    .catchOnly[upickle.Invalid.Data](DatedAppContent.pkl.read(readJson))
                    .leftMap(copInj[UpickleError](_))
                } yield readModel

            })
            .map(program.withProgram(_).sequence)), DomStorage.Local, nonceSource, dataDirKey
      ).detach
      _ <- Task.delay(logger.debug(s"got cache hits: ${output.collect { case Some(p) => p.title }}"))
    } yield output
  }

  def loadContent(configsExtVar: ExternalVar[Map[Int, SlateProgramConfig]], dataDirKey: StorageFS.StorageKey[StorageFS.Dir]): Observable[SearchPageProps] = {
    val configs = configsExtVar.getter()
    val res: Observable[List[AppProps]] = for {
      programOutput <- toObservable(deserializeProgramOutput(configs))
      cachedOutputs <- toObservable(getCachedOutput(configs, dataDirKey, programOutput.map(_.withoutProgram)).map(_.flatten))
      cachedOutputsMapped = cachedOutputs.groupBy(_.id).mapValues(_.head)
      results <- raceFold[AppProps, List[AppProps]](programOutput.collect {
        case SlateProgram(id, title, titleLink, out)
          if cachedOutputsMapped.get(id).forall(
            _.program.forall(d =>
              configs(id).bootRefreshPolicy.shouldRefresh(((js.Date.now() - d.date.getTime()) / 1000.0).toInt)
            )
          ) =>
          val injectedErrors: Task[Either[AllErrors, DatedAppContent]] = out
            .leftMap(copSub[AllErrors](_)).map(_.map(_.leftMap(copSub[AllErrors](_))))
            .sequence[Task, Either[AllErrors, Vector[ExpandableContentModel]]]
            .map((modelsErrErr: Either[AllErrors, Either[AllErrors, Vector[ExpandableContentModel]]]) =>
              modelsErrErr.flatten: AllErrors Either Vector[ExpandableContentModel]
            )
          .map(_.map(models => DatedAppContent(models.toList, new js.Date(js.Date.now()))))
          injectedErrors.flatMap { errorsOrContent =>
            val errMsg = errorsOrContent.fold(_ => "errors", _ => "no errors")
            logger.debug(s"Program $title finished with $errMsg")
            errorsOrContent.traverse[Task, Unit](r =>
              StorageProgram.runProgram(
                new StorageFS(DomStorage.Local, nonceSource, dataDirKey),
                StorageProgram.update(makeDataKey(title, configs(id).input), DatedAppContent.pkl.write(r).toString())
              ).detach
            ).map(_ => AppProps(id, configs(id), title, titleLink, Some(errorsOrContent.map(o => AppModel(o.content, o.date)))))
          }
      })(
        builtinPrograms.map(program =>
          AppProps(program.id, configs(program.id), program.title, program.titleLink, cachedOutputsMapped.get(program.id).map {
              _.program.leftMap(copSub[AllErrors][UpickleError](_)).map(pa => AppModel(pa.content, pa.date))
          }))(collection.breakOut)
      )((l, ap) => ap +: l.filterNot(_.id == ap.id))(6, 400.millis)
      _ = logger.debug(s"refresh, ${results.length} apps loaded")
    } yield results
    res.map { appProps =>
      SearchPageProps(appProps.sortBy(_.id))
    }
  }

  def render(extVar: ExternalVar[Map[Int, SlateProgramConfig]], container: org.scalajs.dom.raw.Element, content: SearchPageProps)(
    implicit scheduler: Scheduler): Task[ReactComponentM[SearchPageProps, Unit, Unit, TopNode]] = {

    val searchPage = DashboardPage.makeDashboardPage(extVar, content)
    Task.create[ReactComponentM[SearchPageProps, Unit, Unit, TopNode]] {
      (_, cb) =>
        logger.debug("Rendering...")
        ReactDOM.render(searchPage, container,
          js.ThisFunction.fromFunction1[ReactComponentM[SearchPageProps, Unit, Unit, TopNode], Unit](t => cb(Success(t))))
        upgradeDom()
        Cancelable.empty
    }
  }

  def upgradeDom(): Unit = {
    val _ = js.Dynamic.global.componentHandler.upgradeDom()
  }

  def loadAndRenderContent(extVar: ExternalVar[Map[Int, SlateProgramConfig]], dataDirKey: StorageFS.StorageKey[StorageFS.Dir], container: dom.Element)(implicit sch: Scheduler): Observable[SearchPageProps] =
    loadContent(extVar, dataDirKey)
      .flatMap(r =>
        toObservable(render(extVar, container, r)).map(_ => r)
      )

  @JSExport
  def main(): Unit = {

    import monix.execution.Scheduler.Implicits.global

    type AppState = Map[Int, SlateProgramConfig]
    val appStore = new Store[AppState](builtinPrograms.map(p => p.id -> p.program._1)(collection.breakOut))

    val container: dom.Element =
      dom.document.body.children.namedItem("container")
    val _ = (for {
      _ <- appendStyles
      dataDirKey <- StorageProgram.runProgram(DomStorage.Local, StorageFS.initFS >> prepareDataFolder).detach
      _ <- loadAndRenderContent(appStore.view, dataDirKey, container).completedL
    } yield ()).runAsync(new monix.eval.Callback[Unit] {
      override def onSuccess(value: Unit): Unit = println("QQ run loop finished successfully!")
      override def onError(ex: Throwable): Unit = println(s"QQ run loop finished with error: $ex")
    })(global)
  }

  // TODO: cache
  private def appendStyles = Task.delay {

    import slate.views._

    val renderer = new StringRenderer.Raw(StringRenderer.formatTiny)
    val addStyles =
      Seq(
        Styles,
        ExpandableContentView.Styles,
        ErrorView.Styles,
        TitledContentView.Styles,
        AppView.Styles
      ).map(_.renderA(renderer)).mkString("\n")
    val aggregateStyles = PlatformExports.createStyleElement(addStyles)
    dom.document.head appendChild aggregateStyles
    logger.debug("appended styles")
  }
}
