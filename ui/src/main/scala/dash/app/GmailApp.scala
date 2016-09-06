package dash
package app

object GmailApp {

  val program =
    """
def headers: {
  Authorization: "Bearer " + googleAuth
};

def listParams: {
  maxResults: 10,
  q: "is:unread"
};

def getParams: {
  format: "metadata",
  metadataHeaders: "Subject",
  fields: "messages(payload/headers,snippet)"
};

def threadList:
  httpGet("https://www.googleapis.com/gmail/v1/users/me/threads"; listParams; ""; headers) | .threads[].id;

def threadDetails:
  [threadList | httpGet("https://www.googleapis.com/gmail/v1/users/me/threads/" + .; getParams; ""; headers)];

def threadToContent: {
  title: "Gmail",
  content: [threadDetails | .[].messages.[0] | {
    title: .payload.headers.[0].value,
    content: .snippet
  }]
};

threadToContent
    """

}
