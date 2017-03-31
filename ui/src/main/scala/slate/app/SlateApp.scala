package slate
package app

import cats._
import cats.implicits._
import japgolly.scalajs.react.{ReactComponentM, ReactDOM, TopNode}
import monix.cats._
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import org.scalajs.dom
import qq.Platform.Rec._
import qq.cc.{CompileError, InterpretedFilter, ParserCompiler, QQCompiler, QQInterpreterRuntime, RuntimeError}
import qq.data.{FilterAST, JSON, Program}
import shapeless.{:+:, CNil}
import slate.app.builtin.{GCalendarApp, GmailApp}
import slate.app.caching.Caching
import slate.app.caching.Program.ErrorGettingCachedProgram
import slate.app.refresh.BootRefreshPolicy
import slate.models.{AppModel, DatedAppContent, ExpandableContentModel}
import slate.storage._
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
import slate.storage.StorageFS.Dir

@JSExport
object SlateApp extends scalajs.js.JSApp {

  private[this] val logger = LoggerFactory.getLogger("DashboarderApp")

  final class EmptyResponseException extends java.lang.Exception("Empty response")

  def builtinPrograms: Vector[SlateProgram[(SlateProgramConfig, InterpretedFilter Either String)]] =
    Vector(
      SlateProgram(1, "Gmail", "https://gmail.com",
        (SlateProgramConfig(JSON.Obj(), BootRefreshPolicy.IfOlderThan(120)), GmailApp.program)),
      SlateProgram(2, "Google Calendar", "https://calendar.google.com",
        (SlateProgramConfig(JSON.Obj(), BootRefreshPolicy.IfOlderThan(60)), GCalendarApp.program))
    )

  type ErrorCompilingPrograms = CompileError :+: ErrorGettingCachedProgram

  def nonceSource(): String = {
    val int = scala.util.Random.nextInt(1000)
    int.toString
  }

  // we assume that `fsroot` exists.

  @inline def makeTopLevelFolder[F[_] : Monad, B](name: String, alreadyPresent: (Dir, StorageFS.Key[Dir]) => B, dirMade: (Dir, StorageFS.Key[Dir]) => B)
                                                 (implicit storage: Storage[F]): F[B] =
    StorageFS.mkDir[F, B](name, nonceSource, StorageFS.fsroot, alreadyPresent, dirMade).map(_.get)

  def compiledPrograms: Task[Vector[SlateProgram[Either[ErrorCompilingPrograms, InterpretedFilter]]]] = {

    implicit val storage: Storage[Task] =
      DomStorage.Local

    def getCompiledProgramIfApplicable[F[_] : Monad](program: SlateProgram[Either[InterpretedFilter, String]])
                                                    (implicit storage: Storage[F]): F[SlateProgram[Either[ErrorGettingCachedProgram, InterpretedFilter]]] = {
      program.program.fold(p =>
        program.withProgram(Either.right[ErrorGettingCachedProgram, InterpretedFilter](p)).pure[F],
        s => parser.program.parse(s).fold(l => Either.left(l), r => Either.right(r)) // caching.Program.getCachedProgramByHash[F](program.withProgram(s)).map(program.withProgram)
      )
    }

    val prog: Alg[Storage, Monad, Vector[SlateProgram[Either[ErrorGettingCachedProgram, Program[FilterAST]]]]] =
      new Alg[Storage, Monad, Vector[SlateProgram[Either[ErrorGettingCachedProgram, Program[FilterAST]]]]] {
        override def apply[G[_] : Monad](tg: Storage[G]): G[Vector[SlateProgram[Either[ErrorGettingCachedProgram, Program[FilterAST]]]]] =
          builtinPrograms.traverse(prog => getCachedProgramIfApplicable[G](prog.map(_._2))(Monad[G], tg))
      }

    for {
      programDirKey <- makeTopLevelFolder[Task, StorageFS.DirKey]("program", (_, k) => k, (_, k) => k)
      cachedPrograms <- StorageFS.runSealedStorageProgram(
        prog,
        DomStorage.Local,
        CompressedStorage[Task],
        nonceSource,
        programDirKey
      )
      errorHandledCompiledPrograms = cachedPrograms.map(_.map(
        _.leftMap(copSub[ErrorCompilingPrograms](_))
          .flatMap {
            QQCompiler.compileProgram(SlatePrelude, _).leftMap(copInj[ErrorCompilingPrograms](_))
          }
      ))
    } yield errorHandledCompiledPrograms
  }

  type ErrorRunningPrograms = RuntimeError :+: CNil

  private def runCompiledPrograms(configs: Map[Int, SlateProgramConfig]): Task[Vector[SlateProgram[ErrorCompilingPrograms Either Task[ErrorRunningPrograms Either Vector[JSON]]]]] =
    compiledPrograms.map {
      _.map {
        program =>
          program.map(
            _.map { compiled =>
              CompiledFilter.run(configs(program.id).input, Map.empty, compiled).map {
                _.leftMap[ErrorRunningPrograms](exs => inl(RuntimeError(exs)))
              }
            }
          )
      }
    }

  type ErrorDeserializingProgramOutput = upickle.Invalid.Json :+: upickle.Invalid.Data :+: ErrorRunningPrograms

  private def
  deserializeProgramOutput(configs: Map[Int, SlateProgramConfig],
                           programs: Vector[SlateProgram[ErrorCompilingPrograms Either Task[ErrorRunningPrograms Either Vector[JSON]]]]
                          ): Vector[SlateProgram[ErrorCompilingPrograms Either Task[ErrorDeserializingProgramOutput Either Vector[ExpandableContentModel]]]] =
    programs.map {
      _.map(_.map(_.map(
        _.leftMap(copSub[ErrorDeserializingProgramOutput](_)).flatMap(
          _.traverse[ErrorDeserializingProgramOutput Either ?, ExpandableContentModel] {
            json =>
              val upickleJson = JSON.JSONToUpickleRec.apply(json)
              try Either.right(ExpandableContentModel.pkl.read(upickleJson)) catch {
                case ex: upickle.Invalid.Data => Either.left(copInj[ErrorDeserializingProgramOutput](ex))
              }
          }
        )
      )))
    }

  type AllErrors = upickle.Invalid.Json :+: upickle.Invalid.Data :+: RuntimeError :+: ErrorCompilingPrograms
  type UpickleError = upickle.Invalid.Json :+: upickle.Invalid.Data :+: CNil

  def makeDataKey(title: String, input: JSON): String =
    title + "|" + input.hashCode()

  def getCachedOutput(configs: Map[Int, SlateProgramConfig],
                      dataDirKey: StorageFS.DirKey, programs: Vector[SlateProgram[Unit]]): Task[Vector[Option[SlateProgram[UpickleError Either DatedAppContent]]]] = {
    for {
      cachedContent <-
      StorageFS.runSealedStorageProgram(
        new Alg[Storage, Monad, Vector[Option[SlateProgram[Either[UpickleError, DatedAppContent]]]]] {
          override def apply[G[_] : Monad](tg: Storage[G]): G[Vector[Option[SlateProgram[UpickleError Either DatedAppContent]]]] =
            programs.traverse(program =>
              Caching.getCachedBy[G, UpickleError, SlateProgram[Unit], DatedAppContent](program)(
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
                })(Monad[G], tg)
                .map(p => program.withProgram(p).sequence)
            )
        }, DomStorage.Local, CompressedStorage[Task], nonceSource, dataDirKey
      )
      _ <- Task.delay(logger.debug(s"got cache hits: ${cachedContent.collect { case Some(p) => p.title }}"))
    } yield cachedContent
  }

  def loadContent(configsExtVar: ExternalVar[Map[Int, SlateProgramConfig]],
                  dataDirKey: StorageFS.DirKey): Observable[SearchPageProps] = {
    val configs = configsExtVar.getter()
    val res: Observable[List[AppProps]] = for {
      programOutput <-
      Observable.fromTask(
        runCompiledPrograms(configs).map(deserializeProgramOutput(configs, _))
      )
      cachedOutputs <- Observable.fromTask(getCachedOutput(configs, dataDirKey, programOutput.map(_.withoutProgram)).map(_.flatten))
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
          val storage =
            CompressedStorage(StorageFS(DomStorage.Local, nonceSource, dataDirKey))
          val cacheContent = injectedErrors.flatMap { errorsOrContent =>
            val errMsg = errorsOrContent.fold(_ => "errors", _ => "no errors")
            logger.debug(s"Program $title finished with $errMsg")
            errorsOrContent.traverse[Task, AllErrors, Unit](r =>
              storage.update(makeDataKey(title, configs(id).input), DatedAppContent.pkl.write(r).toString())
            ).map(_ => AppProps(id, configs(id), title, titleLink, Some(errorsOrContent.map(o => AppModel(o.content, o.date)))))
          }
          cacheContent
      })(
        builtinPrograms.map(program =>
          AppProps(program.id, configs(program.id), program.title, program.titleLink, cachedOutputsMapped.get(program.id).map {
            _.program.leftMap(copSub[AllErrors][UpickleError](_)).map(pa => AppModel(pa.content, pa.date))
          }))(collection.breakOut)
      )((l, ap) => ap +: l.filterNot(_.id == ap.id))(6, 400.millis)
      _ = logger.debug(s"refresh, ${results.length} apps loaded")
    } yield results
    val o: Observable[SearchPageProps] =
      res.map { appProps =>
        SearchPageProps(appProps.sortBy(_.id))
      }
    o
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

  @JSExport
  def main(): Unit = {

    import monix.execution.Scheduler.Implicits.global

    type AppState = Map[Int, SlateProgramConfig]
    // TODO: load from storage
    val appStore = new Store[AppState](builtinPrograms.map(p => p.id -> p.program._1)(collection.breakOut))
    implicit val storage: DomStorage = DomStorage.Local

    val container: dom.Element =
      dom.document.body.children.namedItem("container")
    val loadingThrottle: FiniteDuration = 200.millis
    val run: Task[Unit] =
      for {
        _ <- appendStyles
        root <- StorageFS.getRoot[Task]
        _ <- StorageFS.checkFS[Task](root)
        dataDirKey <- makeTopLevelFolder[Task, StorageFS.DirKey]("data", (_, k) => k, (_, k) => k)
        view = appStore.view
        _ <- loadContent(view, dataDirKey)
          .throttleLast(loadingThrottle)
          .mapTask(r => render(view, container, r).map(_ => r))
          .completedL
      } yield ()
    val _ = run.runAsync(new monix.eval.Callback[Unit] {
      override def onSuccess(value: Unit): Unit = println("QQ run loop finished successfully!")

      override def onError(ex: Throwable): Unit = println(s"QQ run loop finished with error: $ex")
    })(global)
  }

  // TODO: cache?
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
