package dash
package app

object JIRAApp {

  val program =
    raw"""
def authHeaders: { Authorization: "Basic ${Creds.hashedJiraCreds}" };

def extractIssues: .issues[] | {
  url: .self,
  summary: .fields.summary,
  key,
  project: .fields.project.name,
  description: (.fields.description | orElse("") | replaceAll("\n+\\s*"; " ↪ ")),
  status: .fields.status.name
};

def issues: httpPost("https://dashboarder.atlassian.net/rest/api/2/search/"; {};
                     { jql: .jql, maxResults: 10 }; authHeaders + { ("Content-Type"): "application/json" }) | extractIssues;

def extractFilters: .[] | {
  url: .self,
  name,
  owner: .owner.name,
  jql,
  issues: [issues],
  viewUrl
};

def filters: httpGet("https://dashboarder.atlassian.net/rest/api/2/filter/favourite"; {}; {}; authHeaders) | extractFilters;

def contentFromIssue: { title: .status + " - " + .key + " - " + .summary,
                        titleUrl: "https://dashboarder.atlassian.net/browse/" + .key,
                        content: .description };

def contentFromFilter: { title: .name,
                         titleUrl: .viewUrl,
                         content: [.issues[] | contentFromIssue] };

filters | contentFromFilter
"""

}
