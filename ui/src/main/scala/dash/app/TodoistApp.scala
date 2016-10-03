package dash
package app

object TodoistApp {
  val program: String =
"""
def oAuthParams: { scope: "data:read", client_id, state: .tok };
def doOAuth: launchAuth("https://todoist.com/oauth/authorize"; oAuthParams);

(. + doOAuth)
  | ($auth as httpPost("https://todoist.com/oauth/access_token"; {client_id, client_secret, code, redirect_uri}; {}; {}) | .access_token in
     $syncResults as httpPost("https://todoist.com/API/v7/sync"; {token: $auth, sync_token: "*", resource_types: "[\"projects\",\"items\"]"}; {}; {}) in
     $projectNamesById as $syncResults | .projects.[] | { (.id | toString): .name } in
     $items as $syncResults | .items in
     $projectNamesById

  )
"""

}
