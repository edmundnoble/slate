package dash
package app

object TodoistApp {
  val program: String =
raw"""
def oAuthParams: { scope: "data:read", client_id, state: .tok };
def doAuth: print | launchAuth("https://todoist.com/oauth/authorize"; oAuthParams);
def getTokenParams: { client_id, client_secret, code };

doAuth
"""

}
