package slate
package views

import slate.app.SlateApp.AllErrors
import slate.app.ProgramCache.{InvalidBase64, InvalidBytecode, ProgramSerializationException}
import fastparse.all.ParseError
import japgolly.scalajs.react._
import org.scalajs.dom.html.Div
import qq.cc.{QQCompilationException, QQRuntimeException}
import upickle.Invalid

import scalacss.Defaults._

object ErrorView {
  object Styles extends StyleSheet.Inline {

    import dsl._
    import scala.language.postfixOps

    val danger: StyleA = style(
      margin(20 px),
      fontFamily(views.Styles.sanFrancisco)
    )

    val runtimeError: StyleA = style(
      marginTop(10 px),
      fontSize(12 px)
    )
  }

  import vdom.all._

  import scalacss.ScalaCssReact._

  def builder(ex: AllErrors): ReactTagOf[Div] = {
    div(Styles.danger, key := ex.toString.hashCode,
      div(
        ex.eliminate(renderDeserializationError,
          _.eliminate(renderQQRuntimeException,
            _.eliminate(renderQQCompilerException,
              _.eliminate(renderParsingException,
                  _.eliminate(renderSerializationException,
                    _.impossible)))))))
  }

  def renderDeserializationError(err: Invalid.Data): TagMod = {
    "Error deserializing output from QQ program: " + err
  }

  def renderSerializationException(err: ProgramSerializationException): TagMod = {
    "Error getting QQ program from cache: " + err
  }

  def renderParsingException(err: ParseError): TagMod = {
    "Error parsing QQ program: " + err
  }

  def renderQQCompilerException(err: QQCompilationException): TagMod = {
    "Error compiling QQ program: " + err
  }

  def renderQQRuntimeException(err: QQRuntimeException): TagMod = {
    ("Errors running QQ program:": TagMod) +
      div(
        err.errors.map(e => div(Styles.runtimeError, e.message)).toList: _*
      )
  }

}
