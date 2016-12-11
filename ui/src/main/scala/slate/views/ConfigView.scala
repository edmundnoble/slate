package slate
package views

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.DomFrag
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.PublishToOneSubject
import qq.data.JSON._
import qq.data.{JSON, LJSON, SJSON}
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
      overflow.hidden
    )

    val modifierButtons = style(
      display.inline
    )

    val modified = style(
      color(black)
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

  def radioButtons[A](options: Map[String, (A, String)], name: String, default: A, subject: A => Unit): ReactElement = {
    val buttons = options.map { case (optionId, (opt, lab)) =>
      val in: DomFrag =
        input(`type` := "radio", id := s"radio-button-$optionId", `class` := "mdl-radio__button", "name".reactAttr := name,
          onChange --> CallbackTo {
            subject(opt)
          }
        )
      label(`class` := "mdl-radio mdl-js-radio mdl-js-ripple-effect",
        in,
        span(`class` := "mdl-radio__label", lab)
      )
    }
    div(buttons)
  }

  def bootRefreshPolicySelector(subject: BootRefreshPolicy => Unit): ReactElement =
    radioButtons(
      Map(
        "always" -> (BootRefreshPolicy.Always -> "Always"),
        "never" -> (BootRefreshPolicy.Never -> "Never")
      ),
      "bootRefreshPolicy", BootRefreshPolicy.Never, subject)

  def makeTextFieldId(): String = "textField" + scala.util.Random.nextInt(998).toString

  def makeTextField(default: String, modified: Boolean): (Observable[String], ReactElement) = {
    val madeId = makeTextFieldId()
    val publishSubject = PublishToOneSubject[String]
    val field =
      div(`class` := "mdl-textfield mdl-js-textfield", Some(Styles.modified: TagMod).filter(_ => modified),
        input(`class` := "mdl-textfield__input", `type` := "text", id := madeId,
          onChange ==> ((e: ReactEventI) => Callback {
            publishSubject.onNext(e.target.value)
          })),
        label(`class` := "mdl-textfield__label", `for` := madeId, default)
      )
    (publishSubject, field)
  }

  def makeIconFAB(icon: String, cb: Callback): ReactElement = {
    button(`class` := "mdl-button mdl-js-button mdl-button--fab",
      onClick --> cb,
      i(`class` := "material-icons", icon)
    )
  }

  def renderLeaf(l: LJSON, parentModified: Boolean, updateRate: FiniteDuration, subject: JSONModification => Unit)(implicit sch: Scheduler): ReactElement = {
    val (text, field) = makeTextField(JSON.renderLJSON(l), parentModified)
    val _ = text.throttleLast(updateRate).map { str =>
      val parsedLJSON = JSON.parserForLJSON.parse(str)
      // TODO: tell user if they done goofed
      parsedLJSON.fold((_, _, _) => (), (json, _) => subject(SetTo(json)))
    }.runAsyncGetLast
    field
  }

  def makeSubKeyField(index: Int, modified: Boolean, updateRate: FiniteDuration, key: String, subject: JSONModification => Unit)(implicit sch: Scheduler): ReactElement = {
    val (text, field) = makeTextField(key, modified)
    val _ = text.throttleLast(updateRate).map { str =>
      subject(UpdateKey(index, str))
    }.runAsyncGetLast
    field
  }

  def makeModifierButtons[J](origin: JSONOrigin, values: Vector[J], subject: JSONModification => Unit): ReactElement = {
    val addButton = makeIconFAB("add", Callback {
      subject(AddTo(origin))
    })
    val removeButton = makeIconFAB("remove", Callback {
      subject(DeleteFrom(origin))
    })
    val buttons = if (values.nonEmpty) {
      removeButton :: addButton :: Nil
    } else origin match {
      case Bottom => addButton :: Nil
      case Top => Nil
    }
    div(Styles.modifierButtons, buttons)
  }

  def renderBranchF(l: ModifiedJSONF[ModifiedJSON], parentModified: Boolean, updateRate: FiniteDuration, subject: JSONModification => Unit)(implicit sch: Scheduler): ReactElement = {
    val subFields = l match {
      case ObjectF(kvPairs) =>
        Seq[TagMod](
          makeModifierButtons(Top, kvPairs, subject),
          kvPairs.toIterator.zipWithIndex
            .map { case (ObjectEntry(modified, k, v), idx) =>
              makeSubKeyField(idx, parentModified && modified, updateRate, k, subject) +
                (makeSubJSONFields(v, parentModified, updateRate, m => subject(RecIdx(idx, m))): TagMod)
            }.toList,
          makeModifierButtons(Bottom, kvPairs, subject)
        )
      case ArrayF(values) => Seq[TagMod](
        makeModifierButtons(Top, values, subject),
        values.toIterator.zipWithIndex
          .map { case (j, idx) => makeSubJSONFields(j, parentModified, updateRate, m => subject(RecIdx(idx, m))) }
          .toList,
        makeModifierButtons(Bottom, values, subject)
      )
    }
    div(subFields: _*)
  }

  def renderBranch(l: SJSON, parentModified: Boolean, updateRate: FiniteDuration, subject: JSONModification => Unit)(implicit sch: Scheduler): ReactElement = {
    val subFields = l match {
      case JSON.ObjList(kvPairs) =>
        Seq[TagMod](
          makeModifierButtons(Top, kvPairs, subject),
          kvPairs.toIterator.zipWithIndex
            .map { case ((k, v), idx) =>
              makeSubKeyField(idx, parentModified, updateRate, k, subject) +
                (makeSubJSONFields(JSON.fromIsModified(v, parentModified), parentModified, updateRate, m => subject(RecIdx(idx, m))): TagMod)
            }.toList,
          makeModifierButtons(Bottom, kvPairs, subject)
        )
      case JSON.Arr(values) => Seq[TagMod](
        makeModifierButtons(Top, values, subject),
        values.toIterator.zipWithIndex
          .map { case (j, idx) => makeSubJSONFields(JSON.fromIsModified(j, parentModified), parentModified, updateRate, m => subject(RecIdx(idx, m))) }
          .toList,
        makeModifierButtons(Bottom, values, subject)
      )
    }
    div(subFields: _*)
  }

  def makeSubJSONFields(value: ModifiedJSON, parentModified: Boolean, updateRate: FiniteDuration, subject: JSONModification => Unit)(implicit sch: Scheduler): ReactElement = {
    value.resume.fold(renderBranchF(_, parentModified, updateRate, subject), { modifiedOrNot =>
      val isModifiedAsJSON = modifiedOrNot.fold(m => (true, m.asInstanceOf[JSON]), u => (parentModified, u))
      val decomposed = JSON.decompose(isModifiedAsJSON._2)
      decomposed.fold(renderLeaf(_, isModifiedAsJSON._1, updateRate, subject), renderBranch(_, isModifiedAsJSON._1, updateRate, subject))
    })
  }

  def renderForm(json: ModifiedJSON, updateRate: FiniteDuration, subject: JSONModification => Unit)(implicit sch: Scheduler): TagMod = {
    makeSubJSONFields(json, parentModified = false, updateRate, subject)
  }

  def freeformJsonInput(updateRate: FiniteDuration, subject: JSONModification => Unit)(implicit sch: Scheduler): ReactComponentB[extra.ExternalVar[ModifiedJSON], Unit, Unit, TopNode] = {
    ReactComponentB[extra.ExternalVar[ModifiedJSON]]("freeform JSON input form")
      .stateless
      .render_P { p =>
        form(action := "#",
          renderForm(p.value, updateRate, subject)
        )
      }
      .domType[TopNode]
  }

  case class ConfigViewState(modifiedConfig: ModifiedSlateProgramConfig, modifications: List[SlateProgramConfigModification])

  object ConfigViewState {

    import SlateProgramConfig.modificationReusability

    implicit val reusability: Reusability[ConfigViewState] =
      Reusability.by[ConfigViewState, List[SlateProgramConfigModification]](_.modifications)
  }

  def builder(save: SlateProgramConfig => Callback)(implicit sch: Scheduler): ReactComponentB[ConfigViewProps, ConfigViewState, Unit, TopNode] = {
    ReactComponentB[ConfigViewProps]("Expandable content view")
      .initialState_P[ConfigViewState](p => ConfigViewState(ModifiedSlateProgramConfig.unmodified(p.currentConfig), Nil))
      .renderPS { ($, props, state) =>
        val stateInputExtVar: extra.ExternalVar[ModifiedJSON] =
          extra.ExternalVar(state.modifiedConfig.input)(i => $.setState(state.copy(modifiedConfig = state.modifiedConfig.copy(input = i))))
        def addPendingModification(modification: SlateProgramConfigModification): Unit = {
          modification(state.modifiedConfig) match {
            case None =>
              logger.debug(s"config modification failed: tried making modification $modification to ${state.modifiedConfig}")
            case Some(newConfig) =>
              logger.debug(s"successfully made a config modification: $modification, new config: $newConfig")
              $.setState(ConfigViewState(newConfig, modification :: state.modifications)).runNow()
          }
        }
        logger.debug(s"re-rendering config view")
        div(Styles.content,
          bootRefreshPolicySelector(newPolicy => addPendingModification(BootRefreshPolicyModification(newPolicy))),
          freeformJsonInput(1000.millis, { j =>
            addPendingModification(InputModification(j))
          }).build(stateInputExtVar)
        )
      }
      .domType[TopNode]
      .componentDidUpdate(_ => Callback {
        SlateApp.upgradeDom()
      })
      .configure(Reusability.shouldComponentUpdate)
  }

}
