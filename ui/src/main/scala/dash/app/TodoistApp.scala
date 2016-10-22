package dash
package app

import cats.data.Xor
import cats.syntax.xor._
import qq.data.{ConcreteFilter, Program}
import qq.macros.QQStager._

object TodoistApp {
  val program: Program[ConcreteFilter] Xor String =
qq"""
def oAuthParams: { scope: "data:read", client_id, state: .tok };
def doOAuth: launchAuth("https://todoist.com/oauth/authorize"; oAuthParams);

(. + doOAuth)
  | (
     $$syncResults as (httpPost("https://todoist.com/oauth/access_token"; {client_id, client_secret, code, redirect_uri}; {}; {}) | .access_token) |
                      httpPost("https://todoist.com/API/v7/sync"; {token: ., sync_token: "*", resource_types: "[\"projects\",\"items\"]"}; {}; {}) in
     $$projects as $$syncResults | .projects.[] in
     $$items as $$syncResults | .items in
     $$project as $$projects in
       [$$items | .[] | select(.project_id == $$project | .id)] | select(. != [empty]) |
       { title: $$project | .name, content: [.[].content | { title: ., content: "" }] })
""".left

}
