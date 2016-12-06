package slate
package views

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.DomFrag
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalajs.dom.svg.A
import qq.data.JSON._
import qq.data.{JSON, LJSON, SJSON}
import slate.app.{SlateProgramConfig, SlateProgramConfigModification}
import slate.app.refresh.BootRefreshPolicy
import slate.util.{ExternalVar, LoggerFactory}
import slate.views.AppView.AppProps

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

    val animationGroup = new slate.views.ScrollFadeContainer("filter-group")

  }

  import japgolly.scalajs.react.vdom.all._

  import scalacss.ScalaCssReact._

  case class ConfigViewProps(currentConfig: SlateProgramConfig)

  object ConfigViewProps {
    implicit val reusability: Reusability[ConfigViewProps] =
      Reusability.byRefOr_==
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

  def makeTextField(default: String, cb: String => Callback): ReactElement = {
    val madeId = makeTextFieldId()
    div(`class` := "mdl-textfield mdl-js-textfield",
      input(`class` := "mdl-textfield__input", `type` := "text", id := madeId,
        onChange ==> ((e: ReactEventI) => cb(e.target.value))),
      label(`class` := "mdl-textfield__label", `for` := madeId, default)
    )
  }

  def makeIconFAB(icon: String, cb: Callback): ReactElement = {
    button(`class` := "mdl-button mdl-js-button mdl-button--fab",
      onClick --> cb,
      i(`class` := "material-icons", icon)
    )
  }

  def renderLeaf(l: LJSON, modifications: List[JSONModification], subject: JSONModification => Unit): ReactElement = {
    makeTextField(JSON.renderLJSON(l), str => Callback {
      val parsedLJSON = JSON.parserForLJSON.parse(str)
      // TODO: tell user they done goofed
      parsedLJSON.fold((_, _, _) => (), (json, _) => subject(SetTo(json)))
    })
  }

  def makeSubKeyField(index: Int, key: String, subject: JSONModification => Unit): ReactElement =
    makeTextField(key, str => Callback {
      subject(UpdateKey(index, str))
    })

  def renderBranch(l: SJSON, modifications: List[JSONModification], subject: JSONModification => Unit): ReactElement = {
    def makeAddButton[J](origin: JSONOrigin): ReactElement = makeIconFAB("add", Callback {
      subject(AddTo(origin))
    })
    def makeRemoveButton[J](origin: JSONOrigin): ReactElement =
      makeIconFAB("remove", Callback {
        subject(DeleteFrom(origin))
      })
    def makeModifierButtons[J](origin: JSONOrigin, values: Vector[J]): ReactElement = {
      val addButton = makeAddButton[J](origin)
      val removeButton = makeRemoveButton[J](origin)
      val buttons = if (values.nonEmpty) {
        removeButton :: addButton :: Nil
      } else origin match {
        case Bottom => addButton :: Nil
        case Top => Nil
      }
      div(Styles.modifierButtons, buttons)
    }
    div(
      (l match {
        case JSON.ObjList(kvPairs) =>
          Seq(
            makeModifierButtons[(String, JSON)](Top, kvPairs): TagMod,
            kvPairs.toIterator.zipWithIndex.map { case ((k, v), idx) =>
              makeSubKeyField(idx, k, subject) +
                (makeSubJSONFields(v, modifications.zoom(idx), m => subject(RecIdx(idx, m))): TagMod)
            }.toList: TagMod,
            makeModifierButtons[(String, JSON)](Bottom, kvPairs): TagMod
          )
        case JSON.Arr(values) => Seq(
          makeModifierButtons[JSON](Top, values): TagMod,
          values.zipWithIndex.map { case (j, idx) => makeSubJSONFields(j, modifications.zoom(idx), m => subject(RecIdx(idx, m))) }: TagMod,
          makeModifierButtons[JSON](Bottom, values): TagMod
        )
      }): _*
    )
  }

  def makeSubJSONFields(value: JSON, modifications: List[JSONModification], subject: JSONModification => Unit): ReactElement = {
    JSON.decompose(value).fold(renderLeaf(_, modifications, subject), renderBranch(_, modifications, subject))
  }

  def renderForm(json: JSON, modifications: List[JSONModification], subject: JSONModification => Unit): TagMod = {
    makeSubJSONFields(json, modifications, subject)
  }

  def freeformJsonInput(subject: JSONModification => Unit): ReactComponentB[JSON, List[JSONModification], Unit, TopNode] = {
    ReactComponentB[JSON]("freeform JSON input form")
      .initialState(Nil)
      .render_S { s =>
        form(action := "#",
          renderForm(s, subject)
        )
      }
      .domType[TopNode]
  }

  def builder(extVar: ExternalVar[SlateProgramConfig])(implicit sch: Scheduler): ReactComponentB[ConfigViewProps, List[SlateProgramConfigModification], Unit, TopNode] = {
    ReactComponentB[ConfigViewProps]("Expandable content view")
      .initialState[List[SlateProgramConfigModification]](Nil)
      .renderPS { ($, props, state) =>
        div(Styles.content,
          bootRefreshPolicySelector(p => state.copy(bootRefreshPolicy = p), (p: SlateProgramConfig) => {
            logger.debug(s"set new bootRefreshPolicy ${p.bootRefreshPolicy}")
            extVar.setter(p)
            $.setState(p).runNow()
          }),
          freeformJsonInput { j =>
            logger.debug(s"set new input value $j")
            extVar.setter(state.copy(input = j))
            $.setState(state.copy(input = j)).runNow()
          }.build(props.input)
        )
      }
      .domType[TopNode]
  }

}
