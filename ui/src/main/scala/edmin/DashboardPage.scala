package edmin

import SearchPage._
import org.scalajs.dom.raw.HTMLStyleElement

import scala.scalajs.js.Date
import scalacss.FontFace


object SearchPage {

  case class Filter(url: String, name: String, owner: String, jql: String, viewUrl: String)
  object Filter {
    implicit val pkl = upickle.default.macroRW[Filter]
  }

  case class Issue(url: String, key: String, project: String, assignee: Option[String], reporter: Option[String], status: String) //created: Date, updated: Date)
  object Issue {
    implicit val pkl = upickle.default.macroRW[Issue]
  }

  case class SearchResult(filter: Filter, issues: Seq[Issue])
  object SearchResult {
    implicit val pkl = upickle.default.macroRW[SearchResult]
  }

}

import scalacss.Defaults._

object MyStyles extends StyleSheet.Inline {

  import dsl._
  import scala.language.postfixOps

  val filterName = style(
    addClassName("filter-name"),
    fontSize(200 %%)
  )
}


class SearchPage(searchResults: Seq[SearchResult]) {

  import scalatags.JsDom.{TypedTag, all}
  import all._

  import scalacss.ScalatagsCss._

  val htmlFrag = html(
    MyStyles.render[TypedTag[HTMLStyleElement]],
    div(
      searchResults.map { searchResult =>
        div(
          h1(
            a(searchResult.filter.name, href := searchResult.filter.viewUrl),
            `class` := "filter-name"
          ),
          div(
            searchResult.issues.groupBy(_.project).map[TypedTag[_], Vector[TypedTag[_]]] { case (_, issues) =>
              div(
                issues.map { issue =>
                  div(
                    issue.key
                  )
                }
              )
            }(collection.breakOut)
          )
        )
      }
    )
  )
}
