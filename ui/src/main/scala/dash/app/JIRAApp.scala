package dash
package app

import qq.data.{ConcreteFilter, Program}
import qq.macros.QQInterpolator._

object JIRAApp {

  val program: Program[ConcreteFilter] =
    qq"""
def authHeaders: (.username + ":" + .password | b64Encode) | { Authorization: "Basic " + . };

def extractIssues: .issues[] | {
  url: .self,
  summary: .fields.summary,
  key,
  project: .fields.project.name,
  description: (.fields.description | orElse("") | replaceAll("\\n+\\\\s*"; " ↪ ")),
  status: .fields.status.name
};

def issues(auth): httpPost("https://dashboarder.atlassian.net/rest/api/2/search/"; {};
                           { jql, maxResults: 10 };
                           auth + { "Content-Type": "application/json" }) | extractIssues;

def extractFilters(auth): .[] | {
  url: .self,
  name,
  owner: .owner.name,
  jql,
  issues: [issues(auth)],
  viewUrl
};

def contentFromIssue: { title: .status + " - " + .key + " - " + .summary,
                        titleUrl: "https://dashboarder.atlassian.net/browse/" + .key,
                        content: .description };

def contentFromFilter: { title: .name,
                         titleUrl: .viewUrl,
                         content: [.issues[] | contentFromIssue] };

$$auth as authHeaders in
  httpGet("https://dashboarder.atlassian.net/rest/api/2/filter/favourite"; {}; {}; $$auth)
      | extractFilters($$auth)
      | contentFromFilter
"""

}
