package slate
package app

import cats._
import cats.implicits._
import japgolly.scalajs.react.{ReactComponentM, ReactDOM, TopNode}
import monix.cats._
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import org.atnos.eff.syntax.all._
import org.atnos.eff.{Eff, Fx, IntoPoly, Member}
import org.scalajs.dom
import qq.Platform.Rec._
import qq.Platform.Js.sexs
import qq.cc.{CompiledFilter, QQCompilationException, QQCompiler, QQRuntimeException}
import qq.data.{FilterAST, JSON, Program}
import shapeless.{:+:, CNil}
import slate.app.builtin.{GCalendarApp, GmailApp}
import slate.app.caching.Caching
import slate.app.caching.Program.ErrorGettingCachedProgram
import slate.models.{AppModel, DatedAppContent, ExpandableContentModel}
import slate.storage._
import slate.util.LoggerFactory
import slate.util.Util._
import slate.views.AppView.AppProps
import slate.views.DashboardPage
import slate.views.DashboardPage.SearchPageProps

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.util.{Success, Try}
import scalacss.defaults.PlatformExports
import scalacss.internal.StringRenderer
import Observable.{fromTask => toObservable}
import org.atnos.eff.addon.monix._
import slate.storage.StorageAction._storageAction
import slate.storage.StorageFS.{Dir, StrVecWriter}

@JSExport
object SlateApp extends scalajs.js.JSApp {

  private[this] val logger = LoggerFactory.getLogger("DashboarderApp")

  sealed abstract class BootRefreshPolicy {
    def shouldRefresh(secondsAge: Int): Boolean
  }

  object BootRefreshPolicy {

    final case class IfOlderThan(seconds: Int) extends BootRefreshPolicy {
      override def shouldRefresh(secondsAge: Int): Boolean = secondsAge > seconds
    }

    case object Never extends BootRefreshPolicy {
      override def shouldRefresh(secondsAge: Int): Boolean = false
    }

    case object Always extends BootRefreshPolicy {
      override def shouldRefresh(secondsAge: Int): Boolean = true
    }

  }

  final case class SlateProgram[+P](id: Int, title: String, bootRefreshPolicy: BootRefreshPolicy, titleLink: String, program: P, input: JSON) {
    def withProgram[B](newProgram: B): SlateProgram[B] = copy(program = newProgram)

    def withoutProgram: SlateProgram[Unit] = copy(program = ())

    def nameInputHash: Int = (title, input).hashCode
  }

  object SlateProgram {

    implicit def slateProgramTraverse: Traverse[SlateProgram] = new Traverse[SlateProgram] {
      override def map[A, B](fa: SlateProgram[A])(f: (A) => B): SlateProgram[B] = fa.copy(program = f(fa.program))

      override def traverse[G[_], A, B](fa: SlateProgram[A])(f: (A) => G[B])(implicit evidence$1: Applicative[G]): G[SlateProgram[B]] =
        f(fa.program).map(fa.withProgram)

      override def foldLeft[A, B](fa: SlateProgram[A], b: B)(f: (B, A) => B): B =
        f(b, fa.program)

      override def foldRight[A, B](fa: SlateProgram[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        f(fa.program, lb)
    }
  }

  def programs: Vector[SlateProgram[Program[FilterAST] Either String]] = {
    Vector[SlateProgram[Program[FilterAST] Either String]](
      SlateProgram(1, "Gmail", BootRefreshPolicy.Always, "https://gmail.com", GmailApp.program, JSON.ObjMap(Map())),
      SlateProgram(2, "Google Calendar", BootRefreshPolicy.Always, "https://calendar.google.com", GCalendarApp.program, JSON.ObjMap(Map()))
    )
  }

  type ErrorCompilingPrograms = QQCompilationException :+: ErrorGettingCachedProgram

  import shapeless.ops.coproduct.Basis

  def nonceSource(): String = {
    val int = scala.util.Random.nextInt(1000)
    int.toString
  }

  // we assume that `fsroot` exists.

  def makeTopLevelFolder[R: _storageAction, B](name: String, alreadyPresent: (Dir, StorageFS.Key[Dir]) => B, dirMade: (Dir, StorageFS.Key[Dir]) => B): Eff[R, B] =
    StorageFS.mkDir(name, nonceSource, StorageFS.fsroot, alreadyPresent, dirMade).map(_.get)

  def compiledPrograms: Task[Vector[SlateProgram[Either[ErrorCompilingPrograms, CompiledFilter]]]] = {

    def reassembleProgram(program: SlateProgram[Either[Program[FilterAST], String]]): StorageProgram[SlateProgram[Either[ErrorGettingCachedProgram, Program[FilterAST]]]] = {
      program.program.fold[StorageProgram[SlateProgram[Either[ErrorGettingCachedProgram, Program[FilterAST]]]]](p =>
        Eff.pure(program.withProgram(Right[ErrorGettingCachedProgram, Program[FilterAST]](p))),
        s => caching.Program.getCachedProgramByHash(program.withProgram(s)).map(program.withProgram))
    }

    val prog: StorageProgram[Vector[SlateProgram[Either[ErrorGettingCachedProgram, Program[FilterAST]]]]] =
      programs.traverse(reassembleProgram)

    type StorageOrTask = Fx.fx2[StorageAction, TimedTask]

    task.runSequential(
      StorageProgram.runProgram(
        DomStorage.Local,
        makeTopLevelFolder("program", (_, k) => k, (_, k) => k).into[StorageOrTask]
      ).flatMap { programDirKey =>
        StorageFS.runSealedStorageProgram[SW, S, SA, T, T, Vector[SlateProgram[ErrorGettingCachedProgram Either Program[FilterAST]]]](
          prog.into[SW],
          DomStorage.Local,
          CompressedStorageI[T, SA],
          nonceSource,
          programDirKey
        )(implicitly[mem1], implicitly[mem2], implicitly[mem3], implicitly[mem4], implicitly[mem5])
          .map(_.map(_.map(
            _.leftMap(r => Basis[ErrorCompilingPrograms, ErrorGettingCachedProgram].inverse(Right(r)))
              .flatMap {
                QQCompiler.compileProgram(SlatePrelude, _).leftMap(inj[ErrorCompilingPrograms][QQCompilationException])
              }
          )))
      })
  }

  type ErrorRunningPrograms = QQRuntimeException :+: CNil

  private def runCompiledPrograms: Task[Vector[SlateProgram[ErrorCompilingPrograms Either Task[ErrorRunningPrograms Either Vector[JSON]]]]] =
    compiledPrograms.map {
      _.map {
        program =>
          program.map(
            _.map {
              compiled =>
                CompiledFilter.run(program.input, Map.empty, compiled).map {
                  _.leftMap[ErrorRunningPrograms](exs => inl(QQRuntimeException(exs)))
                }
            }
          )
      }
    }

  case class InvalidJSON(str: String) extends Exception

  type ErrorDeserializingProgramOutput = InvalidJSON :+: upickle.Invalid.Data :+: ErrorRunningPrograms

  private def
  deserializeProgramOutput: Task[Vector[SlateProgram[ErrorCompilingPrograms Either Task[ErrorDeserializingProgramOutput Either Vector[ExpandableContentModel]]]]] =
    runCompiledPrograms.map {
      _.map(program =>
        program.map(
          _.map(
            _.map(
              _.leftMap(r => Basis[ErrorDeserializingProgramOutput, ErrorRunningPrograms].inverse(Right(r))).flatMap(
                _.traverse[ErrorDeserializingProgramOutput Either ?, ExpandableContentModel] {
                  json =>
                    val upickleJson = JSON.JSONToUpickleRec.apply(json)
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


  type AllErrors = InvalidJSON :+: upickle.Invalid.Data :+: QQRuntimeException :+: ErrorCompilingPrograms

  val errorCompilingProgramsInAllErrors: Basis[AllErrors, ErrorCompilingPrograms] =
    Basis[AllErrors, ErrorCompilingPrograms]

  val errorDeserializingProgramOutputInAllErrors: Basis[AllErrors, ErrorDeserializingProgramOutput] =
    Basis[AllErrors, ErrorDeserializingProgramOutput]

  def makeDataKey(title: String, input: JSON): String =
    title + "|" + input.hashCode()

  type SW = Fx.fx3[TimedTask, StorageAction, StrVecWriter]
  type SA = Fx.fx2[TimedTask, StrVecWriter]
  type S = Fx.fx2[TimedTask, StorageAction]
  type T = Fx.fx1[TimedTask]
  type mem1 = Member.Aux[StorageAction, SW, SA]
  type mem2 = IntoPoly[T, T]
  type mem3 = Member.Aux[StrVecWriter, SA, T]
  type mem4 = Member.Aux[StrVecWriter, SW, S]
  type mem5 = Member.Aux[StorageAction, S, T]

  def getCachedOutput(dataDirKey: StorageFS.Key[StorageFS.Dir], programs: Vector[SlateProgram[Unit]]): Task[Vector[Option[SlateProgram[InvalidJSON Either DatedAppContent]]]] = {
    for {
      cachedContent <-
      task.runSequential(
        StorageFS.runSealedStorageProgram[SW, S, SA, T, T, Vector[(SlateProgram[Unit], Option[InvalidJSON Either DatedAppContent])]](
          programs.traverseA[SW, (SlateProgram[Unit], Option[InvalidJSON Either DatedAppContent])](p =>
            Caching.getCachedBy[SW, InvalidJSON, SlateProgram[Unit], DatedAppContent](p)(
              prg => makeDataKey(prg.title, prg.input), {
                encodedModels =>
                  Try(upickle.json.read(encodedModels))
                    .toOption.flatMap(DatedAppContent.pkl.read.lift)
                    .toRight(InvalidJSON(encodedModels))
              }
            )
              .map(ms => (p, ms))), DomStorage.Local[T], CompressedStorageI[T, SA], nonceSource, dataDirKey
        )(implicitly[mem1], implicitly[mem2], implicitly[mem3], implicitly[mem4], implicitly[mem5])
      )
      cachedPrograms = cachedContent.map {
        case (p, models) => models.map(p.withProgram)
      }
    } yield cachedPrograms
  }

  def loadContent(dataDirKey: StorageFS.Key[StorageFS.Dir]): Observable[Task[SearchPageProps]] = {
    (for {
      programOutput <- toObservable(deserializeProgramOutput)
      cachedOutputs <- toObservable(getCachedOutput(dataDirKey, programOutput.map(_.withoutProgram)).map(_.flatten))
      cachedOutputsMapped = cachedOutputs.groupBy(_.id).mapValues(_.head)
      results <- raceFold(programOutput.collect {
        case SlateProgram(id, title, bootRefreshPolicy, titleLink, out, input)
          if cachedOutputsMapped.get(id).forall(
            _.program.forall(d =>
              bootRefreshPolicy.shouldRefresh(((js.Date.now() - d.date.getTime()) / 1000.0).toInt)
            )
          ) =>
          val injectedErrors: Task[Either[AllErrors, DatedAppContent]] = out.leftMap(e =>
            errorCompilingProgramsInAllErrors.inverse(Right(e))
          ).map(_.map(_.leftMap(e => errorDeserializingProgramOutputInAllErrors.inverse(Right(e)))))
            .sequence[Task, Either[AllErrors, Vector[ExpandableContentModel]]].map((modelsErrErr: Either[AllErrors, Either[AllErrors, Vector[ExpandableContentModel]]]) =>
            modelsErrErr.flatMap(_.map(models => DatedAppContent(models.toList, new js.Date(js.Date.now()))))
          )
          type mem1 = Member.Aux[StorageAction, Fx.fx2[StorageAction, TimedTask], Fx.fx1[TimedTask]]
          injectedErrors.map(errorsOrContent =>
            errorsOrContent.traverse[Task, AllErrors, Unit](r =>
              task.runSequential(
                StorageProgram.runProgram(
                  CompressedStorage(StorageFS(DomStorage.Local[Fx.fx1[TimedTask]], nonceSource, dataDirKey)),
                  StorageProgram.update(makeDataKey(title, input), DatedAppContent.pkl.write(r).toString()).into[Fx.fx2[StorageAction, TimedTask]]
                )(implicitly[mem1])
              )
            ).map(_ => AppProps(id, input, title, titleLink, Some(errorsOrContent.map(o => AppModel(o.content, o.date)))))
          )
      })(
        runCompiledPrograms.map(
          _.map(t => AppProps(t.id, t.input, t.title, t.titleLink, cachedOutputsMapped.get(t.id).map {
            p =>
              p.program.leftMap(inj[AllErrors].apply[InvalidJSON]).map(pa => AppModel(pa.content, pa.date))
          })))
      )((l, ap) => Task.mapBoth(ap, l) {
        (a, li) => a +: li.filterNot(_.id == a.id)
      })
    } yield results).map(_.map(appProps => SearchPageProps(appProps.sortBy(_.id).toList)))
  }

  def render(container: org.scalajs.dom.raw.Element, content: SearchPageProps)(
    implicit scheduler: Scheduler): Task[ReactComponentM[SearchPageProps, Unit, Unit, TopNode]] = {

    val searchPage =
      DashboardPage
        .makeDashboardPage
        .build(content)
    Task.create {
      (_, cb) =>
        ReactDOM.render(searchPage, container,
          js.ThisFunction.fromFunction1[ReactComponentM[SearchPageProps, Unit, Unit, TopNode], Unit](t => cb(Success(t))))
        Cancelable.empty
    }
  }

  import scala.concurrent.duration._

  def loadAndRenderContent(dataDirKey: StorageFS.Key[StorageFS.Dir], container: dom.Element)(implicit sch: Scheduler): Observable[SearchPageProps] =
    (for {
      compilePrograms <- loadContent(dataDirKey)
      outputs <- toObservable(compilePrograms)
    } yield outputs).throttleLast(500.millis).flatMap(r =>
      toObservable(render(container, r)).map(_ => r)
    )

  @JSExport
  def main(): Unit = {

    import monix.execution.Scheduler.Implicits.global

    val container: dom.Element =
      dom.document.body.children.namedItem("container")
    type S = Fx.fx3[StorageAction, LsAction, TimedTask]
    type S1 = Fx.fx2[StorageAction, TimedTask]
    type T = Fx.fx1[TimedTask]
    type mem1 = Member.Aux[StorageAction, S1, T]
    type mem2 = Member.Aux[LsAction, S, S1]
    val _ = (for {
      _ <- appendStyles
      dataDirKey <- task.runSequential(StorageProgram.runProgramWithLsProgram[S, S1, T, StorageFS.Key[Dir]](DomStorage.Local, DomStorage.LocalLs[S1],
        StorageFS.checkFS[S](None) >> makeTopLevelFolder[S, StorageFS.Key[Dir]]("data", (_, k) => k, (_, k) => k)
      )(implicitly[mem1], implicitly[mem2]))
      _ <- loadAndRenderContent(dataDirKey, container).completedL
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
