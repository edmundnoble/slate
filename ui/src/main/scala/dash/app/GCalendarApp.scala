package dash.app

import qq.data.{ConcreteFilter, Program}
import qq.macros.QQInterpolator._

object GCalendarApp {
  val program: Program[ConcreteFilter] =
qq"""
def authHeaders: { Authorization: "Bearer " + googleAuth };
$$calendarIds as httpGet("https://www.googleapis.com/calendar/v3/users/me/calendarList"; {minAccessRole: "writer"}; {}; authHeaders) | .items.[].id in
$$calendarId as $$calendarIds in {
  title: $$calendarId,
  content: httpGet("https://www.googleapis.com/calendar/v3/calendars/" + $$calendarId + "/events";
                   {maxResults: 10, fields: "items(end,location,organizer,start,status,summary)"}; {}; authHeaders) | [.items.[] | print | {
    title: .summary,
    content: .location | orElse("")
  }]
}"""
}
