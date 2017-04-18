package slate
package views

import cats.data.Ior
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.{Scala, ScalaBuilder}
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.TagOf
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.PublishToOneSubject
import org.scalajs.dom.html.Label
import qq.data.JSON._
import qq.data.{JSON, LJSON}
import shapeless.tag.@@
import slate.app._
import slate.app.refresh.BootRefreshPolicy
import slate.util.LoggerFactory

import scala.concurrent.duration._
import scalacss.Defaults._

object ConfigView {

  private[this] val logger = LoggerFactory.getLogger("ConfigView")

  object Styles extends StyleSheet.Inline {

    import dsl._

    import scala.language.postfixOps

    val panel = style(
      addClassNames("mdl-cell", "mdl-card", "mdl-shadow--2dp", "mdl-cell--6-col", "mdl-color--grey-100", "mdl-color-text--grey-600"),
      marginRight(10 px),
      marginLeft(10 px),
      paddingBottom(20 px),
      paddingLeft(-5 px),
      width :=! "calc(50% - 20px)"
    )

    val header = style(
      addClassName("mdl-card__title"),
      marginTop(-10 px),
      display.inlineBlock
    )

    val linkIcon = style(
      color(c"#303030"),
      addClassName("material-icons"),
      fontSize(24 px),
      marginLeft(6 px),
      marginTop(-7 px),
      verticalAlign.middle,
      (textDecoration := "none").important
    )

    val title = style(
      addClassNames("mdl-card__title-text"),
      color(c"#303030"),
      (textDecoration := "none").important,
      fontWeight._700,
      textOverflow := "ellipsis",
      letterSpacing(1 px),
      fontFamily :=! "Akrobat",
      display inline
    )

    val headerRightDate = style(
      fontSize(10 px),
      marginRight(10 px),
      float right,
      marginTop(10 px),
      marginBottom(10 px)
    )

    val content = style(
      width(100 %%),
      overflow.hidden,
      margin(10 px)
    )

    val modifierButtons = style(
      display.inline
    )

    val modified = style(
      color(black)
    )

    val configSection = style(
      marginBottom(20 px)
    )

    val verticalFlex = style(
      display.flex,
      flexDirection.column,
      flexWrap.wrap
    )

    val buttons = style(
      verticalFlex
    )

    val radioButton = style(
      addClassName("mdl-radio__button"),
      margin(5 px),
      marginTop.`0`
    )

    val configSectionTitle = style(
      marginBottom(5 px)
    )

    val animationGroup = new slate.views.ScrollFadeContainer("filter-group")

  }

  import japgolly.scalajs.react.vdom.all._

  import scalacss.ScalaCssReact._

  case class ConfigViewProps(currentConfig: SlateProgramConfig)

  object ConfigViewProps {
    implicit val reusability: Reusability[ConfigViewProps] =
      Reusability.caseClass
  }

  def radioButtons[A](options: Map[String, (A, String)], name: String, default: A, subject: A => Unit): VdomElement = {
    val buttons: List[TagOf[Label]] = options.map { case (optionId, (opt, lab)) =>
      val in: VdomElement =
        input(Styles.radioButton,
          `type` := "radio", id := s"radio-button-$optionId", VdomAttr[String]("name") := name,
          onChange --> CallbackTo {
            subject(opt)
          }
        )
      label(`class` := "mdl-radio mdl-js-radio mdl-js-ripple-effect",
        in,
        span(`class` := "mdl-radio__label", lab)
      )
    }(collection.breakOut)
    div((Styles.buttons: TagMod) :: buttons: _*)
  }

  def bootRefreshPolicySelector(subject: BootRefreshPolicy => Unit): VdomElement = {
    radioButtons(
      Map(
        "always" -> (BootRefreshPolicy.Always -> "Always"),
        "never" -> (BootRefreshPolicy.Never -> "Never")
      ),
      "bootRefreshPolicy", BootRefreshPolicy.Never, subject
    )
  }

  def makeTextFieldId(): String = {
    "textField" + scala.util.Random.nextInt(998).toString
  }

  def makeTextField(default: Ior[String @@ Modified, String]): (Observable[String], VdomElement) = {
    val madeId = makeTextFieldId()
    val publishSubject = PublishToOneSubject[String]
    val field =
      div(`class` := "mdl-textfield mdl-js-textfield",
        input(`class` := "mdl-textfield__input", `type` := "text", id := madeId, TagMod.fromTraversableOnce(default.right.map(value := _)),
          onChange ==> ((e: ReactEventFromInput) => Callback {
            publishSubject.onNext(e.target.value)
          })),
        label(`class` := "mdl-textfield__label", `for` := madeId, TagMod.fromTraversableOnce(default.left.map(s => s: TagMod)))
      )
    (publishSubject, field)
  }

  def makeIconFAB(icon: String, cb: Callback): VdomElement = {
    button(`class` := "mdl-button mdl-js-button mdl-button--fab",
      onClick --> cb,
      i(`class` := "material-icons", icon)
    )
  }

  def renderLeaf(l: Ior[LJSON @@ Modified, LJSON], updateRate: FiniteDuration, subject: JSONModification => Unit)(implicit sch: Scheduler): VdomElement = {
    val (text, field) = makeTextField(l.bimap(mj => modifiedTag(JSON.renderLJSON(mj)), JSON.renderLJSON))
    val _ = text.throttleLast(updateRate).map { str =>
      val parsedLJSON = JSON.parserForLJSON.parse(str)
      // TODO: tell user if they done goofed
      parsedLJSON.fold((_, _, _) => (), (json, _) => subject(SetTo(json)))
    }.runAsyncGetLast
    field
  }

  def makeSubKeyField(index: Int, updateRate: FiniteDuration, key: Ior[String @@ Modified, String], subject: JSONModification => Unit)(implicit sch: Scheduler): VdomElement = {
    val (text, field) = makeTextField(key)
    val _ = text.throttleLast(updateRate).map { str =>
      subject(UpdateKey(index, str))
    }.runAsyncGetLast
    field
  }

  def makeModifierButtons[J](origin: JSONOrigin, values: Vector[J], subject: JSONModification => Unit): VdomElement = {
    val addButton = makeIconFAB("add", Callback {
      subject(AddTo(origin))
    })
    val removeButton = makeIconFAB("remove", Callback {
      subject(DeleteFrom(origin))
    })
    val buttons: List[TagMod] = if (values.nonEmpty) {
      removeButton :: addButton :: Nil
    } else origin match {
      case Bottom => addButton :: Nil
      case Top => Nil
    }
    div((Styles.modifierButtons: TagMod) :: buttons: _*)
  }

  def renderBranchF(l: ModifiedJSONF[ModifiedJSON], updateRate: FiniteDuration, subject: JSONModification => Unit)(implicit sch: Scheduler): VdomElement = {
    l match {
      case ObjectF(kvPairs) =>
        div(((Styles.verticalFlex: TagMod) ::
          makeModifierButtons(Top, kvPairs, subject) ::
          kvPairs.toIterator.zipWithIndex
            .flatMap { case (ObjectEntry(k, v), idx) =>
              makeSubKeyField(idx, updateRate, k, subject) ::
                (makeSubJSONFields(v, updateRate, m => subject(RecIdx(idx, m))): TagMod) :: Nil
            }.toList) :+
          makeModifierButtons(Bottom, kvPairs, subject): _*
        )
      case ArrayF(values) =>
        div(((Styles.verticalFlex: TagMod) ::
          makeModifierButtons(Top, values, subject) ::
          values.toIterator.zipWithIndex
            .map { case (j, idx) => makeSubJSONFields(j, updateRate, m => subject(RecIdx(idx, m))) }
            .toList) :+
          makeModifierButtons(Bottom, values, subject): _*
        )
    }
  }

  def makeSubJSONFields(value: ModifiedJSON, updateRate: FiniteDuration, subject: JSONModification => Unit)(implicit sch: Scheduler): VdomElement = {
    value.resume.fold(renderBranchF(_, updateRate, subject), renderLeaf(_, updateRate, subject))
  }

  def renderForm(json: ModifiedJSON, updateRate: FiniteDuration, subject: JSONModification => Unit)(implicit sch: Scheduler): TagMod = {
    makeSubJSONFields(json, updateRate, subject)
  }

  def freeformJsonInput(updateRate: FiniteDuration, json: ModifiedJSON, subject: JSONModification => Unit)
                       (implicit sch: Scheduler): Scala.Unmounted[Unit, Unit, Unit] = {
    Scala.builder[Unit]("freeform JSON input form")
      .renderStatic {
        form(action := "#",
          renderForm(json, updateRate, subject)
        )
      }
      .build.apply()
  }

  case class ConfigViewState(modifiedConfig: ModifiedSlateProgramConfig, changedShape: Boolean)

  def builder(save: SlateProgramConfig => Callback)(implicit sch: Scheduler): ScalaBuilder.Step4[ConfigViewProps, Children.None, ConfigViewState, Unit] = {
    def addPendingModification(state: ConfigViewState, setState: ConfigViewState => Unit, modification: SlateProgramConfigModification): Unit = {
      modification(state.modifiedConfig) match {
        case None =>
          logger.debug(s"config modification failed: tried making modification $modification to ${state.modifiedConfig}")
        case Some(newConfig) =>
          logger.debug(s"successfully made a config modification: $modification, new input: ${JSON.render(commit(newConfig.input))}")
          setState(ConfigViewState(newConfig, modification.changesShape))
      }
    }

    Scala.builder[ConfigViewProps]("Expandable content view")
      .initialState_P[ConfigViewState](p => ConfigViewState(ModifiedSlateProgramConfig.unmodified(p.currentConfig), changedShape = false))
      .render { $ =>
        val setter = (st: ConfigViewState) => $.setState(st).runNow()
        logger.debug(s"re-rendering config view with state ${$.state}")
        div(Styles.content,
          div(Styles.configSection,
            div(Styles.configSectionTitle, "Refresh on startup?"),
            bootRefreshPolicySelector(newPolicy => addPendingModification($.state, setter, BootRefreshPolicyModification(newPolicy)))
          ),
          div(Styles.configSection,
            div(Styles.configSectionTitle, "App input"),
            freeformJsonInput(1000.millis, $.state.modifiedConfig.input, { j =>
              addPendingModification($.state, setter, InputModification(j))
            })
          )
        )
      }
      .componentDidUpdate(_ => Callback {
        SlateApp.upgradeDom()
      })
      .shouldComponentUpdate(u => CallbackTo {
        u.nextState.changedShape
      })
  }

}
