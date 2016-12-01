package slate
package views

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.DomFrag
import monix.execution.Scheduler
import monix.reactive.Observable
import qq.data.JSON._
import qq.data.{JSON, LJSON, SJSON}
import slate.app.SlateProgramConfig
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

    val animationGroup = new slate.views.ScrollFadeContainer("filter-group")

  }

  import japgolly.scalajs.react.vdom.all._

  import scalacss.ScalaCssReact._

  case class ConfigViewOut(reactElement: ReactComponentB[AppProps, Unit, Unit, TopNode],
                           config: Observable[SlateProgramConfig])

  case class ConfigViewProps(defaultConfig: SlateProgramConfig)

  object ConfigViewProps {
    implicit val reusability: Reusability[ConfigViewProps] =
      Reusability.byRefOr_==
  }

  def radioButtons[A, B](options: Map[String, (A, String)], name: String, default: A, inject: A => B, subject: B => Unit): ReactElement = {
    val buttons = options.map { case (optionId, (opt, lab)) =>
      val in: DomFrag =
        input(`type` := "radio", id := s"radio-button-$optionId", `class` := "mdl-radio__button", "name".reactAttr := name,
          onChange --> CallbackTo {
            subject(inject(opt))
          }
        )
      label(`class` := "mdl-radio mdl-js-radio mdl-js-ripple-effect",
        in,
        span(`class` := "mdl-radio__label", lab)
      )
    }
    div(buttons)
  }

  def bootRefreshPolicySelector[A](inject: BootRefreshPolicy => A, subject: A => Unit): ReactElement =
    radioButtons[BootRefreshPolicy, A](
      Map(
        ("always", (BootRefreshPolicy.Always, "Always")),
        ("never", (BootRefreshPolicy.Never, "never"))
      ),
      "bootRefreshPolicy", BootRefreshPolicy.Never, inject, subject)

  def makeTextFieldId(): String = scala.util.Random.nextInt(100).toString

  def makeTextField(default: String, cb: String => Callback): ReactElement = {
    div(`class` := "mdl-textfield mdl-js-textfield",
      input(`class` := "mdl-textfield__input", `type` := "text", value := default,
        onChange ==> ((e: ReactEventI) => cb(e.target.value)))
    )
  }

  def makeIconFAB(icon: String, cb: Callback): ReactElement = {
    button(`class` := "mdl-button mdl-js-button mdl-button--fab",
      onClick --> cb,
      i(`class` := "material-icons", icon)
    )
  }

  def renderLeaf(l: LJSON, lens: LJSON => JSON, subject: JSON => Unit): ReactElement =
    makeTextField(JSON.renderLJSON(l), str => Callback {
      subject(lens(JSON.parserForLJSON.parse(str).get.value))
    })

  def makeSubKeyField(key: String, mlens: String => JSON, subject: JSON => Unit): ReactElement =
    makeTextField(key, str => Callback {
      subject(mlens(str))
    })

  def renderBranch(l: SJSON, lens: SJSON => JSON, subject: JSON => Unit): ReactElement = {
    def makeAddButton[J](origin: JSONOrigin, addDefault: J, values: Vector[J], construct: Vector[J] => JSON): ReactElement = makeIconFAB("add", Callback {
      subject(lens(construct(if (origin == Top) values :+ addDefault else addDefault +: values)))
    })
    def makeRemoveButton[J](origin: JSONOrigin, values: Vector[J], construct: Vector[J] => JSON): ReactElement =
      makeIconFAB("remove", Callback {
        subject(lens(construct(if (origin == Top) values.init else values.tail)))
      })
    def makeModifierButtons[J](origin: JSONOrigin, values: Vector[J], addDefault: J, construct: Vector[J] => JSON): Seq[ReactElement] = {
      val addButton = makeAddButton[J](origin, addDefault, values, construct)
      val removeButton = makeRemoveButton[J](origin, values, construct)
      if (values.nonEmpty) {
        removeButton :: addButton :: Nil
      } else origin match {
        case Bottom => addButton :: Nil
        case Top => Nil
      }
    }
    val defaultArrayElement = JSON.str("")
    val defaultObjectElement = "" -> JSON.str("")
    div(
      l match {
        case JSON.ObjList(kvPairs) =>
          Seq(
            makeModifierButtons(Top, kvPairs, defaultObjectElement, JSON.ObjList(_)): TagMod,
            kvPairs.toIterator.zipWithIndex.map { case ((k, v), idx) =>
              makeSubKeyField(k, kn => JSON.ObjList(kvPairs.updated(idx, kn -> v)), subject) +
                (makeSubJSONFields(v, vn => JSON.ObjList(kvPairs.updated(idx, k -> vn)), subject): TagMod)
            }.toList: TagMod,
            makeModifierButtons(Bottom, kvPairs, defaultObjectElement, JSON.ObjList(_)): TagMod
          )
        case JSON.Arr(values) => Seq(
          makeModifierButtons(Top, values, defaultArrayElement, JSON.Arr(_)): TagMod,
          values.zipWithIndex.map { case (j, idx) => makeSubJSONFields(j, jn => JSON.Arr(values.updated(idx, jn)), subject) }: TagMod,
          makeModifierButtons(Bottom, values, defaultArrayElement, JSON.Arr(_)): TagMod
        )
      }
    )
  }

  def makeSubJSONFields(value: JSON, mlens: JSON => JSON, subject: JSON => Unit): ReactElement = {
    JSON.decompose(value).fold(renderLeaf(_, mlens, subject), renderBranch(_, mlens, subject))
  }

  def renderForm(json: JSON, subject: JSON => Unit): TagMod = {
    makeSubJSONFields(json, identity, subject)
  }

  def freeformJsonInput(subject: JSON => Unit): ReactComponentB[JSON, JSON, Unit, TopNode] = {
    ReactComponentB[JSON]("freeform JSON input form")
      .initialState_P(identity)
      .render_S { s =>
        form(action := "#",
          renderForm(s, subject)
        )
      }
      .domType[TopNode]
  }

  def builder(extVar: ExternalVar[SlateProgramConfig])(implicit sch: Scheduler): ReactComponentB[ConfigViewProps, SlateProgramConfig, Unit, TopNode] = {
    ReactComponentB[ConfigViewProps]("Expandable content view")
      .initialState_P(_.defaultConfig)
      .renderS { ($, state) =>
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
          }.build(state.input)
        )
      }
      .domType[TopNode]
  }

}
