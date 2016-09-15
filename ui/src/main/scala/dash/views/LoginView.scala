package dash
package views

object LoginView {

  val responseType = "token"
  val clientId = Creds.googleClientId
  def validChar: Char = (scala.util.Random.nextInt('Z' - '0') - '0').toChar
  def nonce = List.fill(20)(validChar)

}
