package slate
package app

import cats._
import cats.implicits._
import diode.ActionResult.{ModelUpdate, NoChange}
import diode.react.ReactConnector
import diode.{Monad => _, _}
import japgolly.scalajs.react.Callback
import monix.cats._
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import org.scalajs.dom
import qq.Platform.Rec._
import qq.cc.{CompiledFilter, QQCompilationException, QQCompiler, QQRuntimeException}
import qq.data.{FilterAST, JSON, Program}
import shapeless.{:+:, CNil}
import slate.app.builtin.{GCalendarApp, GmailApp, TodoistApp}
import slate.app.caching.Caching
import slate.app.caching.Program.ErrorGettingCachedProgram
import slate.app.refresh.BootRefreshPolicy
import slate.models.{DatedAppContent, ExpandableContentModel}
import slate.storage._
import slate.util.{ExternalVar, LoggerFactory}
import slate.util.Util._
import slate.views.DashboardPage
import slate.views.DashboardPage.SearchPageProps

import scala.concurrent.duration._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scalacss.defaults.PlatformExports
import scalacss.internal.StringRenderer
import slate.storage.StorageFS.Dir

import scala.scalajs.js.UndefOr

@JSExport
object SlateApp extends scalajs.js.JSApp {

  implicit val storage: Storage[Task] =
    CompressedStorage(StorageFS(DomStorage.Local, nonceSource, StorageFS.fsroot))

  private[this] val logger =
    LoggerFactory.getLogger("DashboarderApp")

  final class EmptyResponseException extends java.lang.Exception("Empty response")

  def builtinPrograms: List[SlateProgram[(SlateProgramConfig, Program[FilterAST] Either String)]] = {
    List(
      SlateProgram(1, "Gmail", "https://gmail.com",
        (SlateProgramConfig(JSON.Obj(), BootRefreshPolicy.IfOlderThan(120)), GmailApp.program)),
      SlateProgram(2, "Google Calendar", "https://calendar.google.com",
        (SlateProgramConfig(JSON.Obj(), BootRefreshPolicy.IfOlderThan(60)), GCalendarApp.program)),
      SlateProgram(3, "Todoist", "https://todoist.com",
        (SlateProgramConfig(JSON.Obj(), BootRefreshPolicy.IfOlderThan(60)), TodoistApp.program))
    )
  }

  type ErrorCompilingPrograms = QQCompilationException :+: ErrorGettingCachedProgram

  def nonceSource(): String = {
    val int = scala.util.Random.nextInt(1000)
    int.toString
  }

  // we assume that `fsroot` exists.

  @inline def makeTopLevelFolder[F[_] : Monad, B](name: String, alreadyPresent: (Dir, StorageFS.Key[Dir]) => B, dirMade: (Dir, StorageFS.Key[Dir]) => B)
                                                 (implicit storage: Storage[F]): F[B] = {
    StorageFS.mkDir[F, B](name, nonceSource, StorageFS.fsroot, alreadyPresent, dirMade).map(_.get)
  }

  def getCachedProgramIfApplicable[F[_] : Monad](program: SlateProgram[Either[Program[FilterAST], String]])
                                                (implicit storage: Storage[F]): F[ErrorGettingCachedProgram Either Program[FilterAST]] = {
    program.program.fold(p =>
      Either.right[ErrorGettingCachedProgram, Program[FilterAST]](p).pure[F],
      s => caching.Program.getCachedProgramByHash[F](program.withProgram(s)))
  }

  def compileProgram(config: SlateProgramConfig, program: SlateProgram[Program[FilterAST] Either String]
                    ): Task[Either[ErrorCompilingPrograms, CompiledFilter]] = {
    for {
      programDirKey <- makeTopLevelFolder[Task, StorageFS.DirKey]("program", (_, k) => k, (_, k) => k)
      storage = new StorageFS(CompressedStorage(DomStorage.Local), nonceSource, programDirKey)
      cachedProgram <- getCachedProgramIfApplicable[Task](program)(Monad[Task], storage)
      errorHandledCompiledPrograms =
      cachedProgram.leftMap(copSub[ErrorCompilingPrograms](_))
        .flatMap {
          QQCompiler.compileProgram(SlatePrelude, _).leftMap(copInj[ErrorCompilingPrograms](_))
        }
    } yield errorHandledCompiledPrograms
  }

  type ErrorRunningPrograms = QQRuntimeException :+: CNil

  private def runCompiledProgram(config: SlateProgramConfig, program: SlateProgram[CompiledFilter]): SlateProgram[Task[ErrorRunningPrograms Either Vector[JSON]]] = {
    program.map { compiled =>
      CompiledFilter.run(config.input, Map.empty, compiled).map {
        _.leftMap[ErrorRunningPrograms](exs => inl(QQRuntimeException(exs)))
      }
    }
  }

  type ErrorDeserializingProgramOutput = upickle.Invalid.Json :+: upickle.Invalid.Data :+: ErrorRunningPrograms

  private def
  deserializeProgramOutput(json: JSON): AllErrors Either ExpandableContentModel = {
    val upickleJson = JSON.JSONToUpickleRec.apply(json)
    try ExpandableContentModel.pkl.read(upickleJson).asRight catch {
      case ex: upickle.Invalid.Data => copInj[AllErrors](ex).asLeft
    }
  }

  type AllErrors = upickle.Invalid.Json :+: upickle.Invalid.Data :+: QQRuntimeException :+: ErrorCompilingPrograms
  type UpickleError = upickle.Invalid.Json :+: upickle.Invalid.Data :+: CNil

  def makeDataKey(title: String, input: JSON): String = {
    title + "|" + input.hashCode()
  }

  def getSavedConfig(program: SlateProgram[Unit], configDirKey: StorageFS.DirKey): Task[Option[UpickleError Either SlateProgramConfig]] = {
    Caching.getCachedBy(program)(_.title, { encodedModels =>
      for {
        readJson <-
        Either.catchOnly[upickle.Invalid.Json](upickle.json.read(encodedModels))
          .leftMap(copInj[UpickleError](_))
        readModel <- Either
          .catchOnly[upickle.Invalid.Data](SlateProgramConfig.pkl.read(readJson))
          .leftMap(copInj[UpickleError](_))
      } yield readModel
    })
  }

  def saveConfig(program: SlateProgram[Unit], configDirKey: StorageFS.DirKey, config: SlateProgramConfig): Task[Option[UpickleError Either Unit]] = {
    ???
  }

  def getCachedOutput(config: SlateProgramConfig,
                      dataDirKey: StorageFS.DirKey,
                      program: SlateProgram[Unit])
                     (implicit storage: Storage[Task]): Task[Option[SlateProgram[UpickleError Either DatedAppContent]]] = {
    for {
      cachedContent <-
      Caching.getCachedBy(program)(
        prg => makeDataKey(prg.title, config.input), {
          encodedModels =>
            for {
              readJson <- Either
                .catchOnly[upickle.Invalid.Json](upickle.json.read(encodedModels))
                .leftMap(copInj[UpickleError](_))
              readModel <- Either
                .catchOnly[upickle.Invalid.Data](DatedAppContent.pkl.read(readJson))
                .leftMap(copInj[UpickleError](_))
            } yield readModel
        })
        .map(program.withProgram(_).sequence[Option, UpickleError Either DatedAppContent])
      _ <- Task.delay {
        val message =
          if (cachedContent.isEmpty) "miss" else "hit"
        logger.debug("got cache " + message + ": " + program.title)
      }
    } yield cachedContent
  }

  def loadContent(programs: List[SlateProgram[(SlateProgramConfig, Task[AllErrors Either DatedAppContent])]],
                  dispatcher: Dispatcher,
                  dataDirKey: StorageFS.DirKey): Task[Unit] = {
    Task.gatherUnordered(programs.map {
      case slateProgram@SlateProgram(id, _, _, (config, program)) =>
        for {
          cachedOutput <- getCachedOutput(config, dataDirKey, slateProgram.void)
          _ <- cachedOutput.traverse[Task, Unit](ea =>
            dispatcher.send(ContentLoaded(id, ea.program.leftMap(copSub[AllErrors](_)), cached = true))
          )
          shouldFetch = cachedOutput.forall(cached =>
            cached.program.forall(c =>
              config.bootRefreshPolicy.shouldRefresh(((js.Date.now() - c.date.getTime()) / 1000.0).toInt)
            )
          )
          _ <- if (shouldFetch)
            program.flatMap(ea =>
              dispatcher.send(ContentLoaded(id, ea, cached = true))
            )
        } yield ()
        program.flatMap(ea => dispatcher.send(ContentLoaded(id, ea, cached = false)))
    }).map(_ => ())
  }

  /*
    val res: Task[Unit] = for {
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
  */

  def render(extVar: ExternalVar[Map[Int, SlateProgramConfig]], container: org.scalajs.dom.raw.Element, content: SearchPageProps)(
    implicit scheduler: Scheduler): Task[Unit] = {

    val searchPage = DashboardPage.makeDashboardPage(extVar, content)
    Task.create[Unit] {
      (_, cb) =>
        logger.debug("Rendering...")
        searchPage.renderIntoDOM(container, Callback(cb.onSuccess(())))
        upgradeDom()
        Cancelable.empty
    }
  }

  def upgradeDom(): Unit = {
    val _ = js.Dynamic.global.componentHandler.upgradeDom()
  }

  /**
    * States.
    */
  final case class ProgramState(config: SlateProgramConfig,
                                unsavedConfig: ModifiedSlateProgramConfig,
                                qq: ErrorCompilingPrograms Either CompiledFilter,
                                appContent: Option[AllErrors Either DatedAppContent])

  final case class AppState(programs: Map[Int, SlateProgram[ProgramState]])

  /**
    * Actions.
    */
  final case class ConfigAltered(id: Int, configEdits: SlateProgramConfigModification) extends Action

  final case class SaveConfig(id: Int) extends Action

  final case class ContentLoaded(id: Int, content: AllErrors Either DatedAppContent, cached: Boolean) extends Action

  final case object BootComplete extends Action

  implicit final class dispatcherOps(val dispatcher: Dispatcher) extends AnyVal {
    def send[T: ActionType](action: T): Task[Unit] = {
      Task.delay(dispatcher.dispatch(action))
    }
  }

  final class AppCircuit(container: dom.Element, initialState: AppState)
    extends Circuit[AppState]
      with ReactConnector[AppState] {

    import scala.scalajs.js.JSConverters._

    def zoomConfig(index: Int): ModelR[AppState, UndefOr[SlateProgramConfig]] = {
      zoom(_.programs.get(index).orUndefined.map(_.program.config))
    }

    def withProgram(state: AppState, id: Int, errMsg: => String)(f: SlateProgram[ProgramState] => ActionResult[AppState]): ActionResult[AppState] = {
      state.programs.get(id).fold[ActionResult[AppState]] {
        logger.error(errMsg)
        NoChange
      }(f)
    }

    def alterState(id: Int, f: SlateProgram[ProgramState] => ProgramState): AppState => ActionResult[AppState] = {
      s => ModelUpdate(s.programs.get(id).fold(s)(p => s.copy(programs = s.programs + (id -> p.coflatMap(f)))))
    }

    override protected def actionHandler: HandlerFunction = {
      case (state, ConfigAltered(id, edit)) =>
        Some(withProgram(state, id, s"altered config for non-existent app with id $id") { program =>
          edit(program.program.unsavedConfig).fold[ActionResult[AppState]] {
            logger.error("config modification failed for app with id $id")
            NoChange
          } { newConfig =>
            alterState(id, _.program.copy(unsavedConfig = newConfig))(state)
          }
        })
      case (state, SaveConfig(id)) =>
        Some(withProgram(state, id, s"saved config for non-existent app with id $id") { program =>
          alterState(id, prg => prg.program.copy(config = prg.program.unsavedConfig.commit))(state)
        })
      case (state, BootComplete) =>
        // TODO
        Some(NoChange)
      case (state, ContentLoaded(id, content, cached)) =>
        Some(withProgram(state, id, s"loaded (cached: $cached) content for non-existent app with id $id") { program =>
          if (content.isLeft && program.program.appContent.forall(_.isRight)) {
            // TODO: report
            NoChange
          } else {
            alterState(id, prg => prg.program.copy(appContent = Some(content)))(state)
          }
        })
      case (_, unknown) =>
        logger.error(s"unknown action: $unknown")
        None
    }

    override protected val initialModel: AppState =
      initialState
  }

  def makeCircuit(initialState: AppState, container: dom.Element): Task[AppCircuit] = {
    Task.delay {
      new AppCircuit(container, initialState)
    }
  }

  val loadingThrottle: FiniteDuration = 200.millis

  @JSExport
  def main(): Unit = {

    import monix.execution.Scheduler.Implicits.global
    // TODO: load from storage
    val container: dom.Element =
      dom.document.body.children.namedItem("container")
    val run: Task[Unit] =
      for {
        _ <- appendStyles
        root <- StorageFS.getRoot[Task]
        _ <- StorageFS.checkFS[Task](root)
        dataDirKey <- makeTopLevelFolder[Task, StorageFS.DirKey]("data", (_, k) => k, (_, k) => k)
        configDirKey <- makeTopLevelFolder[Task, StorageFS.DirKey]("config", (_, k) => k, (_, k) => k)
        // TODO: load these
        rawPrograms <- Task.now(builtinPrograms)
        initialState <-
        rawPrograms.traverse[Task, (Int, SlateProgram[ProgramState])] { program =>
          val (config, prg) = program.program
          val compiledProgram = compileProgram(config, program.withProgram(prg))
          compiledProgram.map(p => program.id -> program.withProgram(ProgramState(config, ModifiedSlateProgramConfig.unmodified(config), p, None)))
        }.map(e => AppState(e.toMap))
        circuit <- makeCircuit(initialState, container)
        programs = builtinPrograms.map {
          case slateProgram@SlateProgram(_, _, _, (config, _)) =>
            slateProgram.withProgram((config, for {
              compiledProgram <- compileProgram(config, slateProgram.map(_._2))
              ranProgram <- compiledProgram.traverse[Task, ErrorCompilingPrograms, Either[ErrorRunningPrograms, Vector[JSON]]](o =>
                runCompiledProgram(config, slateProgram.withProgram(o)).program
              )
              deserializedOutput = ranProgram
                .leftMap(copSub[AllErrors](_))
                .flatMap(_.leftMap(copSub[AllErrors](_)))
                .flatMap(e => e.traverse[AllErrors Either ?, ExpandableContentModel](
                  deserializeProgramOutput(_).leftMap(copSub[AllErrors](_))
                ).map(v => DatedAppContent(v.toList, new js.Date(js.Date.now()))))
            } yield deserializedOutput))
        }
        _ <- loadContent(programs, circuit, dataDirKey)
      } yield ()
    val _ = run.runAsync(new monix.eval.Callback[Unit] {
      override def onSuccess(value: Unit): Unit = {
        println("QQ run loop finished successfully!")
      }

      override def onError(ex: Throwable): Unit = {
        println(s"QQ run loop finished with error: $ex")
      }
    })(global)
  }

  // TODO: cache?
  private def appendStyles = {
    Task.delay {

      import slate.views._

      val renderer = new StringRenderer.Raw(StringRenderer.formatTiny)
      val addStyles =
        Seq(
          Styles,
          ExpandableContentView.Styles,
          ErrorView.Styles,
          TitledContentView.Styles,
          ConfigView.Styles,
          AppView.Styles
        ).map(_.renderA(renderer)).mkString("\n")
      val aggregateStyles = PlatformExports.createStyleElement(addStyles)
      dom.document.head appendChild aggregateStyles
      logger.debug("appended styles")
    }
  }
}
