package slate
package views

import slate.app.DashboarderApp.ErrorDeserializingProgramOutput
import slate.app.ProgramCache.{InvalidBase64, InvalidBytecode}
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

  def builder(ex: ErrorDeserializingProgramOutput): ReactTagOf[Div] = {
    div(Styles.danger, key := ex.toString.hashCode,
      div(
        ex.eliminate(renderDeserializationError,
          _.eliminate(renderQQRuntimeException,
            _.eliminate(renderQQCompilerException,
              _.eliminate(renderParsingException,
                _.eliminate(renderBase64Exception,
                  _.eliminate(renderBytecodeException,
                    _.impossible))))))))
  }

  def renderDeserializationError(err: Invalid.Data): TagMod = {
    "Error deserializing output from QQ program: " + err
  }

  def renderBytecodeException(err: InvalidBytecode): TagMod = {
    "Error getting QQ program from cache (bytecode): " + err
  }

  def renderBase64Exception(err: InvalidBase64): TagMod = {
    "Error getting QQ program from cache (base64): " + err
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
