package dash.views

import dash.Creds

object LoginView {

  val responseType = "token"
  val clientId = Creds.clientId
  def validChar: Char = (scala.util.Random.nextInt('Z' - '0') - '0').toChar
  def nonce = List.fill(20)(validChar)

}
