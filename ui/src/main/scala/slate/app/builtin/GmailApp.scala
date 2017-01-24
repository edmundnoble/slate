package slate
package app
package builtin

import qq.data.{FilterAST, Program}
import qq.macros.stager.QQStager._

object GmailApp {

  val program: Program[FilterAST] Either String =
    Left(
      qq"""
def authHeaders: { Authorization: "Bearer " + googleAuth };

def listUnreadThreadParams: {
  maxResults: 10,
  q: "is:unread"
};

def getThreadDetailsParams: {
  format: "metadata",
  metadataHeaders: "Subject",
  fields: "messages(payload/headers,snippet)"
};

def unreadThreadDetails:
  $$auth as authHeaders in
    httpGet("https://www.googleapis.com/gmail/v1/users/me/threads"; listUnreadThreadParams; ""; $$auth) |
    .threads[].id |
    httpGet("https://www.googleapis.com/gmail/v1/users/me/threads/" + .; getThreadDetailsParams; ""; $$auth);

def unreadThreadDetailsToContent: {
  title: "Unread threads",
  content: [.[].messages.[0] | {
    title: .payload.headers.[0].value,
    content: .snippet
  }]
};

[unreadThreadDetails] | unreadThreadDetailsToContent
    """
    )

}
