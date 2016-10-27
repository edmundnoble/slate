package dash.app

import cats.data.Xor
import qq.data.{ConcreteFilter, Program}
import qq.macros.QQStager._
import cats.syntax.xor._

object TaigaApp {

  val program: Program[ConcreteFilter] Xor String =
    """
$authHeaders as httpPost("https://api.taiga.io/api/v1/auth"; {}; {password, username, type:"normal"}; {Content-Type: "application/json"}) | {Authorization: "Bearer " + .auth_token} in
$project as httpGet("https://api.taiga.io/api/v1/projects"; {order_by: "total_activity_last_week"}; {}; $authHeaders) | .[] | select(.i_am_member) in
$storiesList as httpGet("https://api.taiga.io/api/v1/userstories"; {project: $project | .id, status__is_closed: false}; {}; $authHeaders) in
  [{
    title: $project | .name,
    content: [
      storiesList | .[] | httpGet("https://api.taiga.io/api/v1/userstories/" + (.id | toString); {}; {}; $authHeaders) | {
        title: .subject,
        content: .description
      }
    ]
  }]
      """.right

}
