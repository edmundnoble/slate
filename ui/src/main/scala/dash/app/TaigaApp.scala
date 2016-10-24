package dash.app

import cats.data.Xor
import qq.data.{ConcreteFilter, Program}
import qq.macros.QQStager._
import cats.syntax.xor._

object TaigaApp {

  val program: Program[ConcreteFilter] Xor String =
    qq"""
def authInfo: { username, password, type: "normal" };
def loginResult: httpPost("https://api.taiga.io/api/v1/auth"; {}; authInfo; {Content-Type: "application/json"});
def authHeaders: { Authorization: "Bearer " + (loginResult | .auth_token)  };

$$auth as authHeaders in


      """.left

}
