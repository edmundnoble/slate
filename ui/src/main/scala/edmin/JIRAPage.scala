package edmin

import SearchPage._
import japgolly.scalajs.react._
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLStyleElement

import scala.scalajs.js.Date
import scalacss.Defaults._
import scalacss.{ClassName, Css, NonEmptyVector, Pseudo, Renderer, StyleA, StyleS}
import japgolly.scalajs.react.Addons.ReactCssTransitionGroup
import japgolly.scalajs.react.CompScope.DuringCallbackU
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.TagMod
import monocle.{Fold, Lens}
import monocle.macros.{GenLens, Lenses}

import scala.concurrent.duration._
import scalaz.Const

object SearchPage {

  import japgolly.scalajs.react.vdom.prefix_<^._
  import scala.language.implicitConversions

  import scalacss.ScalaCssReact._

  case class Filter(url: String, name: String, owner: String, jql: String, viewUrl: String)
  object Filter {
    implicit val pkl = upickle.default.macroRW[Filter]
  }

  case class Issue(url: String, summary: String, key: String, project: String, assignee: Option[String], reporter: Option[String],
                   status: String, description: String, created: Double, updated: Double)
  object Issue {
    implicit val pkl = upickle.default.macroRW[Issue]
  }

  case class SearchResult(filter: Filter, issues: Seq[Issue])
  object SearchResult {
    implicit val pkl = upickle.default.macroRW[SearchResult]
  }

  def issueTemplate(issue: Issue) =
    ReactComponentB[Unit]("Issue")
      .render(_ =>
        <.div(Styles.filterIssue,
          <.div(Styles.filterIssueHeader,
            <.a(
              issue.status + " - " + issue.key + " - " + issue.summary,
              ^.href := "https://auviknetworks.atlassian.net/browse/" + issue.key
            )
          ),
          <.div(Styles.filterIssueDescription,
            s"${issue.description}"
          )
        )
      )
      .build()

  def single[A](op: Option[A]): Seq[A] =
    op.fold(Vector.empty[A])(Vector.empty[A] :+ _)

  def filterRowTemplate(firstResult: Option[SearchResult], secondResult: Option[SearchResult]) =
    <.div(Styles.filterContainer,
      <.div(Styles.innerFilterContainer,
        single(firstResult.map(filterCardTemplate))
      ),
      <.div(Styles.innerFilterContainer,
        single(secondResult.map(filterCardTemplate))
      )
    )


  case class FilterState(expanded: Boolean)

  object FilterState {
    val expanded = GenLens[FilterState](_.expanded)
  }

  val ReactTagOfModifiers = Lens((_: ReactTagOf[_]).modifiers)( c => e => e.copy(modifiers = c))

  def filterCardTemplate(result: SearchResult) = {
    import monocle.state.all._
    import MonocleReact._
    val toggleExpanded = FilterState.expanded.modify(!_)
    def buttonStyleForState(filterState: FilterState): Seq[TagMod] = {
      val otherStyles = Vector[TagMod](Styles.filterButtonIcon, "expand_more")
      if (filterState.expanded) otherStyles.:+[TagMod, Vector[TagMod]](Styles.filterButtonExpanded)
      else otherStyles
    }
    ReactComponentB[Unit]("JIRA filter card")
      .initialState(FilterState(expanded = false))
      .renderS { ($: DuringCallbackU[Unit, FilterState, Unit], state) =>
        <.div(Styles.filter,
          <.div(Styles.filterHeader,
            <.div(Styles.filterHeaderLeft,
              <.div(Styles.filterNumber,
                <.a(result.issues.length.toString, ^.href := result.filter.viewUrl)),
              <.div(Styles.filterName,
                <.a(result.filter.name, ^.href := result.filter.viewUrl)
              )
            ),
            <.button(Styles.filterButton,
              ^.onClick --> $.modState(toggleExpanded),
              <.i(buttonStyleForState(state): _*)
            )
          ),
          Styles.filterIssueContainer(
            (if (state.expanded) {
              <.div(
                ^.key := result.filter.url,
                result.issues.sortBy(_.updated).map(issueTemplate)
              ) +: Vector.empty
            } else {
              Vector.empty[ReactTag]
            }): _*
          )
        )
      }
      .build()
  }

  def makeSearchPage(searchResults: Seq[SearchResult]) = {
    val htmlFrag =
      <.div(
        <.div(Styles.appBar,
          <.table(
            <.tr(Styles.appBarRow,
              <.td(Styles.appBarText,
                "Dashboarder")
            )
          )
        ),
        <.div(
          <.div(Styles.searchResults,
            Styles.searchResultContainer(
              if (searchResults.nonEmpty) {
                searchResults.grouped(2).map(xs => filterRowTemplate(xs.headOption, xs.tail.headOption)).toSeq
              } else {
                Vector.empty[ReactNode]
              }
            )
          )
        )
      )

    ReactComponentB[Unit]("JIRA search page")
      .render(_ => htmlFrag)
      .build()
  }
}

object Styles extends StyleSheet.Inline {

  import dsl._
  import scala.language.postfixOps

  val roboto = scalacss.FontFace("Roboto", src = NonEmptyVector("https://fonts.googleapis.com/css?family=Roboto"))
  val robotoLight = scalacss.FontFace("Roboto", src = NonEmptyVector("https://fonts.googleapis.com/css?family=Roboto:300"))
  val robotoMedium = scalacss.FontFace("Roboto", src = NonEmptyVector("https://fonts.googleapis.com/css?family=Roboto:700"))
  val robotoHeavy = scalacss.FontFace("Roboto", src = NonEmptyVector("https://fonts.googleapis.com/css?family=Roboto:900"))

  val materialBlue = c"#2196F3"
  val materialGrey = c"#757575"

  val pseudoContent = content := "\"\""

  val appBar = style(
    addClassName("mui-appbar")
  )

  val appBarRow = style(
    verticalAlign.middle
  )

  val appBarText = style(
    addClassNames("mui--appbar-height", "mui--text-display1"),
    paddingLeft(10 px),
    fontFamily(robotoLight)
  )

  val searchResults = style(
    addClassName("mui-container-fluid"),
    width(100 %%),
    fontFamily(roboto),
    color(materialBlue),
    marginTop(30 px)
  )

  val innerFilterContainer = style(
    addClassName("mui-col-md-6")
  )

  val filterContainer = style(
    addClassName("mui-row")
  )

  val filter = style(
    addClassName("mui-panel"),
    overflow.hidden,
    marginBottom(40 px)
  )

  val filterHeader = style(
    addClassName("mui--divider-bottom"),
    width(100 %%),
    display.inlineBlock
  )

  val filterButton = style(
    addClassNames("mui-btn", "mui-btn--raised"),
    float.right,
    lineHeight.`0`.important
  )

  val filterButtonIcon = style(
    addClassName("material-icons"),
    (transition := "transform 0.3s ease-out").important
  )

  val filterButtonExpanded = style(
    transform := "rotateX(-180deg)",
    perspective(600 pt)
  )

  val filterHeaderLeft = style(
    float left,
    marginTop(10 px),
    marginBottom(10 px)
  )

  val filterNumber = style(
    addClassName("mui--text-headline"),
    paddingRight(10 px),
    display inline
  )

  val filterName = style(
    addClassName("mui--text-headline"),
    fontFamily(robotoHeavy),
    display inline
    //fontSize(150 %%)
  )

  val filterIssue = style(
    display contents,
    pageBreakInside.avoid,
    position relative,
    marginBottom(10 px),
    &.after(
      height(1.2 em),
      bottom(0 px),
      position absolute,
      textAlign right,
      pseudoContent,
      width(100 %%),
      backgroundImage := "linear-gradient(rgba(255, 255, 255, 0), rgba(255, 255, 255, 1))"
    )
  )

  abstract class ReactAnimationStyles(val className: String)(implicit r: scalacss.mutable.Register) extends StyleSheet.Inline()(r) {
    def enterClassName = className + "-enter"
    def leaveClassName = className + "-leave"
    def enterActiveClassName = className + "-enter-active"
    def leaveActiveClassName = className + "-leave-active"
    val enter: StyleA
    val leave: StyleA
    val enterActive: StyleA
    val leaveActive: StyleA
    def apply(children: ReactNode*): ReactComponentU_ = ReactCssTransitionGroup(className)(children)
  }

  class ScrollFadeContainer(clasz: String)(implicit r: scalacss.mutable.Register) extends ReactAnimationStyles(clasz)(r) {

    val enter: StyleA = style(enterClassName)(
      maxHeight.`0`,
      opacity(0)
    )

    val leave: StyleA = style(leaveClassName)(
      maxHeight(1200 px),
      opacity(100)
    )

    val enterActive: StyleA = style(enterActiveClassName)(
      maxHeight(1200 px),
      opacity(100),
      transitionProperty := "maxHeight opacity",
      transitionDuration(300.millis),
      transitionTimingFunction.easeInOut
    )

    val leaveActive: StyleA = style(leaveActiveClassName)(
      maxHeight.`0`,
      opacity(0),
      transitionProperty := "maxHeight opacity",
      transitionDuration(300.millis),
      transitionTimingFunction.easeInOut
    )

  }

  val filterIssueContainer = new ScrollFadeContainer("filterIssueContainer")
  val searchResultContainer = new ScrollFadeContainer("searchResultContainer")

  val filterIssueDescription = style(
    addClassName("mui--text-body1"),
    color(black),
    overflow.hidden,
    maxHeight(3.2 em),
    minHeight(2.4 em)
  )

  val filterIssueHeader = style(
    addClassName("mui--text-title"),
    whiteSpace.nowrap,
    overflow.hidden,
    position.relative,
    marginBottom(5 px),
    &.after(
      height(1.2 em),
      bottom(0 px),
      right(0 px),
      position.absolute,
      pseudoContent,
      textAlign.right,
      backgroundImage := "linear-gradient(left, rgba(255, 255, 255, 0), rgba(255, 255, 255, 1))",
      width(10 %%)
    )
  )

}

object LinkStyles extends StyleSheet.Standalone {

  import dsl._

  "a" - color(c"#616161")
}
