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
import slate.storage.{DomStorage, StorageAction, StorageFS, StorageProgram}
import slate.util.{ExternalVar, LoggerFactory}
import slate.util.Util._
import slate.views.AppView.AppProps
import slate.views.DashboardPage
import slate.views.DashboardPage.SearchPageProps
import scala.concurrent.duration._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.util.{Success, Try}
import scalacss.defaults.PlatformExports
import scalacss.internal.StringRenderer

@JSExport
object SlateApp extends scalajs.js.JSApp {

  private[this] val logger = LoggerFactory.getLogger("DashboarderApp")

  final class EmptyResponseException extends java.lang.Exception("Empty response")

  def builtinPrograms: Vector[SlateProgram[(SlateProgramConfig, Program[FilterAST] Either String)]] =
    Vector(
      SlateProgram(1, "Gmail", "https://gmail.com",
        (SlateProgramConfig(JSON.Obj(), BootRefreshPolicy.IfOlderThan(120)), GmailApp.program)),
      SlateProgram(2, "Google Calendar", "https://calendar.google.com",
        (SlateProgramConfig(JSON.Obj(), BootRefreshPolicy.IfOlderThan(60)), GCalendarApp.program))
    )

  type ErrorCompilingPrograms = QQCompilationException :+: ErrorGettingCachedProgram

  import shapeless.ops.coproduct.Basis

  def nonceSource(): String = {
    val int = scala.util.Random.nextInt(1000)
    int.toString
  }

  def prepareProgramFolder: StorageProgram[StorageFS.StorageKey[StorageFS.Dir]] = for {
    programDirKey <- StorageFS.mkDir("program", nonceSource, StorageFS.fsroot)
  } yield programDirKey.get.fold(identity[StorageFS.StorageKey[StorageFS.Dir]])

  def compiledPrograms: Task[Vector[SlateProgram[Either[ErrorCompilingPrograms, CompiledFilter]]]] = {

    def reassembleProgram(program: SlateProgram[Either[Program[FilterAST], String]]): StorageProgram[SlateProgram[Either[ErrorGettingCachedProgram, Program[FilterAST]]]] = {
      program.program.fold[StorageProgram[SlateProgram[Either[ErrorGettingCachedProgram, Program[FilterAST]]]]](p =>
        Eff.pure(program.withProgram(Right[ErrorGettingCachedProgram, Program[FilterAST]](p))),
        s => caching.Program.getCachedProgramByHash(program.withProgram(s)).map(program.withProgram))
    }

    val prog: StorageProgram[Vector[SlateProgram[Either[ErrorGettingCachedProgram, Program[FilterAST]]]]] =
      builtinPrograms.map(_.map(_._2)).traverse(reassembleProgram)

    type StorageOrTask = Fx.fx2[StorageAction, Task]

    StorageProgram.runProgramInto(DomStorage.Local, prepareProgramFolder.into[StorageOrTask]).flatMap { programDirKey =>
      StorageFS.runSealedStorageProgramInto(prog.into[StorageOrTask], DomStorage.Local, nonceSource, programDirKey)
        .map(_.map(_.map(_.leftMap(r => Basis[ErrorCompilingPrograms, ErrorGettingCachedProgram].inverse(Right(r)))
          .flatMap {
            QQCompiler.compileProgram(SlatePrelude, _).leftMap(inj[ErrorCompilingPrograms][QQCompilationException])
          }
        )))
    }.detach
  }

  type ErrorRunningPrograms = QQRuntimeException :+: CNil

  private def runCompiledPrograms(configs: Map[Int, SlateProgramConfig]): Task[Vector[SlateProgram[ErrorCompilingPrograms Either Task[ErrorRunningPrograms Either Vector[JSON]]]]] =
    compiledPrograms.map(
      programs =>
        programs.map(
          program =>
            program.map(
              _.map {
                compiled =>
                  val config = configs(program.id)
                  CompiledFilter.run(config.input, Map.empty, compiled).map(
                    _.leftMap[ErrorRunningPrograms](exs => inl(QQRuntimeException(exs)))
                  )
              }
            )
        )
    )

  case class InvalidJSON(str: String) extends Exception

  type ErrorDeserializingProgramOutput = InvalidJSON :+: upickle.Invalid.Data :+: ErrorRunningPrograms

  private def
  deserializeProgramOutput(configs: Map[Int, SlateProgramConfig]): Task[Vector[SlateProgram[ErrorCompilingPrograms Either Task[ErrorDeserializingProgramOutput Either Vector[ExpandableContentModel]]]]] =
    runCompiledPrograms(configs).map {
      _.map(program =>
        program.map(
          _.map[Task[ErrorDeserializingProgramOutput Either Vector[ExpandableContentModel]]](
            _.map(
              _.leftMap(r => Basis[ErrorDeserializingProgramOutput, ErrorRunningPrograms].inverse(Right(r))).flatMap(
                _.traverse[ErrorDeserializingProgramOutput Either ?, ExpandableContentModel] {
                  json =>
                    val upickleJson = JSON.JSONToUpickleRec(json)
                    try Either.right(ExpandableContentModel.pkl.read(upickleJson)) catch {
                      case ex: upickle.Invalid.Data => Either.left(inj[ErrorDeserializingProgramOutput](ex))
                    }
                }
              )
            )
          )
        )
      )
    }

  def prepareDataFolder: StorageProgram[StorageFS.StorageKey[StorageFS.Dir]] = for {
    dataDirKey <- StorageFS.mkDir("data", nonceSource, StorageFS.fsroot)
  } yield dataDirKey.get.fold(identity[StorageFS.StorageKey[StorageFS.Dir]])

  type AllErrors = InvalidJSON :+: upickle.Invalid.Data :+: QQRuntimeException :+: ErrorCompilingPrograms

  val errorCompilingProgramsInAllErrors: Basis[AllErrors, ErrorCompilingPrograms] =
    Basis[AllErrors, ErrorCompilingPrograms]

  val errorDeserializingProgramOutputInAllErrors: Basis[AllErrors, ErrorDeserializingProgramOutput] =
    Basis[AllErrors, ErrorDeserializingProgramOutput]

  def makeDataKey(title: String, input: JSON): String =
    title + "|" + input.hashCode()

  def getCachedOutput(configs: Map[Int, SlateProgramConfig], dataDirKey: StorageFS.StorageKey[StorageFS.Dir], programs: Vector[SlateProgram[Unit]]): Task[Vector[Option[SlateProgram[InvalidJSON Either DatedAppContent]]]] = {
    for {
      cachedContent <- StorageFS.runSealedStorageProgramInto(
        programs.traverseA(p =>
          Caching.getCachedBy[Fx.fx2[Task, StorageAction], InvalidJSON, SlateProgram[Unit], DatedAppContent](p)(
            prg => makeDataKey(prg.title, configs(prg.id).input), {
              encodedModels =>
                Try(upickle.json.read(encodedModels))
                  .toOption.flatMap(DatedAppContent.pkl.read.lift)
                  .toRight(InvalidJSON(encodedModels))
            })
            .map(ms => (p, ms))), DomStorage.Local, nonceSource, dataDirKey
      ).detach
      cachedPrograms = cachedContent.map {
        case (p, models) => models.map(p.withProgram)
      }
    } yield cachedPrograms
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
          val input = configs(id).input
          val injectedErrors: Task[Either[AllErrors, DatedAppContent]] = out.leftMap(e =>
            errorCompilingProgramsInAllErrors.inverse(Right(e))
          ).map(_.map(_.leftMap(e => errorDeserializingProgramOutputInAllErrors.inverse(Right(e)))))
            .sequence[Task, Either[AllErrors, Vector[ExpandableContentModel]]].map((modelsErrErr: Either[AllErrors, Either[AllErrors, Vector[ExpandableContentModel]]]) =>
            modelsErrErr.flatMap(_.map(models => DatedAppContent(models.toList, new js.Date(js.Date.now()))))
          )
          injectedErrors.flatMap { errorsOrContent =>
            logger.debug(s"Program $title finished, are there errors? ${errorsOrContent.isLeft}")
            errorsOrContent.traverse[Eff[Fx.fx1[Task], ?], Unit](r =>
              StorageProgram.runProgram(
                new StorageFS(DomStorage.Local, nonceSource, dataDirKey),
                StorageProgram.update(makeDataKey(title, input), DatedAppContent.pkl.write(r).toString())
              )
            ).detach.map(_ => AppProps(id, configs(id), title, titleLink, Some(errorsOrContent.map(o => AppModel(o.content, o.date)))))
          }
      })(
        builtinPrograms.map(program =>
          AppProps(program.id, configs(program.id), program.title, program.titleLink, cachedOutputsMapped.get(program.id).map {
            p =>
              logger.debug(s"Cache hit for program ${program.title}")
              p.program.leftMap(inj[AllErrors].apply[InvalidJSON]).map(pa => AppModel(pa.content, pa.date))
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
        js.Dynamic.global.componentHandler.upgradeDom()
        Cancelable.empty
    }
  }

  def loadAndRenderContent(extVar: ExternalVar[Map[Int, SlateProgramConfig]], dataDirKey: StorageFS.StorageKey[StorageFS.Dir], container: dom.Element)(implicit sch: Scheduler): Observable[SearchPageProps] =
    loadContent(extVar, dataDirKey)
      .flatMap(r =>
        toObservable(render(extVar, container, r)).map(_ => r)
      )

  @JSExport
  def main(): Unit = {

    import monix.execution.Scheduler.Implicits.global

    // TODO: can I do this less evil-ly?
    var appState: Map[Int, SlateProgramConfig] = builtinPrograms.map(p => p.id -> p.program._1)(collection.breakOut)
    val extVar = ExternalVar[Map[Int, SlateProgramConfig]](
      () => appState,
      s => appState = s
    )

    val container: dom.Element =
      dom.document.body.children.namedItem("container")
    val _ = (for {
      _ <- appendStyles
      dataDirKey <- StorageProgram.runProgram(DomStorage.Local, StorageFS.initFS >> prepareDataFolder).detach
      _ <- loadAndRenderContent(extVar, dataDirKey, container).completedL
    } yield ()).runAsync(new monix.eval.Callback[Unit] {
      override def onSuccess(value: Unit): Unit = println("QQ run loop finished successfully!")
      override def onError(ex: Throwable): Unit = println(s"QQ run loop finished with error: $ex")
    })
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
  }
}
