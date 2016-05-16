package edmin

import SearchPage._
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLStyleElement

import scala.scalajs.js.Date
import scalacss.{Css, NonEmptyVector, Renderer}
import scalatags.generic.{Aliases, Modifier}


object SearchPage {

  case class Filter(url: String, name: String, owner: String, jql: String, viewUrl: String)
  object Filter {
    implicit val pkl = upickle.default.macroRW[Filter]
  }

  case class Issue(url: String, summary: String, key: String, project: String, assignee: Option[String], reporter: Option[String],
                   status: String, description: String)
  //created: Date, updated: Date)
  object Issue {
    implicit val pkl = upickle.default.macroRW[Issue]
  }

  case class SearchResult(filter: Filter, issues: Seq[Issue])
  object SearchResult {
    implicit val pkl = upickle.default.macroRW[SearchResult]
  }

}

import scalacss.Defaults._

object Styles extends StyleSheet.Inline {

  import dsl._
  import scala.language.postfixOps

  val roboto = scalacss.FontFace("Roboto", src = NonEmptyVector("https://fonts.googleapis.com/css?family=Roboto"))
  val robotoMedium = scalacss.FontFace("Roboto", src = NonEmptyVector("https://fonts.googleapis.com/css?family=Roboto:700"))
  val robotoHeavy = scalacss.FontFace("Roboto", src = NonEmptyVector("https://fonts.googleapis.com/css?family=Roboto:900"))

  val materialBlue = c"#2196F3"
  val materialGrey = c"#757575"

  val pseudoContent = content := "\"\""

  val searchResultContainer = style(
    //    addClassName("mui-container")
  )

  val searchResults = style(
    addClassName("mui-container"),
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

  val filterSpaced = style(
//    addClassName("mui-col-md-offset-1"),
    filter
  )

  val filterName = style(
    addClassNames("mui--divider-bottom", "mui--text-headline"),
    fontFamily(robotoHeavy),
    marginTop(10 px),
    marginBottom(10 px)
    //fontSize(150 %%)
  )

  val filterIssue = style(
    display.contents,
    pageBreakInside.avoid,
    position.relative,
    marginBottom(10 px),
    &.after(
      height(1.2 em),
      bottom(0 px),
      position.absolute,
      textAlign.right,
      pseudoContent,
      width(100 %%),
      backgroundImage := "linear-gradient(rgba(255, 255, 255, 0), rgba(255, 255, 255, 1))"
    )
  )

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

  val filterIssueContainer = style(
  )

  val linkStyle = style(
    addClassName("mui--text-dark-secondary")
  )

}


class SearchPage(searchResults: Seq[SearchResult]) {

  import scalatags.JsDom.{TypedTag, all}
  import all._
  import scala.language.implicitConversions

  import scalacss.ScalatagsCss._

  def styleTag = Styles.render[TypedTag[HTMLStyleElement]]

  def styledLink(mods: scalatags.JsDom.Modifier*) =
    a(styleaToJsDomTag(Styles.linkStyle) +: mods: _*)

  def issueTemplate(issue: Issue) =
    div(Styles.filterIssue,
      div(Styles.filterIssueHeader,
        styledLink(issue.status + " - " + issue.key + " - " + issue.summary,
          href := ("https://auviknetworks.atlassian.net/browse/" + issue.key))
      ),
      div(Styles.filterIssueDescription,
        s"${issue.description}"
      )
    )


  def filterRowTemplate(firstResult: Option[SearchResult], secondResult: Option[SearchResult]) =
    div(Styles.filterContainer,
      div(Styles.innerFilterContainer,
        firstResult.map(filterCardTemplate)
      ),
      div(Styles.innerFilterContainer,
        secondResult.map(filterCardTemplate)
      )
    )

  def filterCardTemplate(result: SearchResult) =
    div(Styles.filter,
      div(Styles.filterName,
        styledLink(result.filter.name, href := result.filter.viewUrl)
      ),
      div(
        result.issues.map(issueTemplate)
      )
    )

  val htmlFrag = div(
    styleTag,
    div(Styles.searchResultContainer,
      div(
        Styles.searchResults,
        searchResults.grouped(2).map(xs => filterRowTemplate(xs.headOption, xs.tail.headOption)).toSeq
      )
    )
  )

}
