package dash
package app

object GmailApp {

  val program =
    raw"""
def authHeaders: { Authorization: "Bearer " + googleAuth };

def unreadThreadParams: {
  maxResults: 10,
  q: "is:unread"
};

def getParams: {
  format: "metadata",
  metadataHeaders: "Subject",
  fields: "messages(payload/headers,snippet)"
};

def listUnreadThreads:
  httpGet("https://www.googleapis.com/gmail/v1/users/me/threads"; unreadThreadParams; ""; authHeaders) |
    .threads[].id;

def unreadThreadDetails:
  listUnreadThreads |
    httpGet("https://www.googleapis.com/gmail/v1/users/me/threads/" + .; getParams; ""; authHeaders);

def unreadThreadsToContent: {
  title: "Unread Threads",
  content: [.[].messages.[0] | {
    title: .payload.headers.[0].value,
    content: .snippet
  }]
};

[unreadThreadDetails] | unreadThreadsToContent
    """

}
