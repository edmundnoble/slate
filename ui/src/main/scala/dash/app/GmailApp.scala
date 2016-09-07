package dash
package app

object GmailApp {

  val program =
    raw"""
def authHeaders: { Authorization: "Bearer " + googleAuth };

def listParams: { maxResults: 10, q: "is:unread" };

def getParams: {
  format: "metadata",
  metadataHeaders: "Subject",
  fields: "messages(payload/headers,snippet)"
};

def threadList:
  httpGet("https://www.googleapis.com/gmail/v1/users/me/threads"; listParams; ""; authHeaders) | .threads[].id;

def threadDetails:
  [threadList | httpGet("https://www.googleapis.com/gmail/v1/users/me/threads/" + .; getParams; ""; authHeaders)];

def threadToContent: {
  title: "Unread Threads",
  content: [threadDetails | .[].messages.[0] | {
    title: .payload.headers.[0].value,
    content: .snippet
  }]
};

threadToContent
    """

}
