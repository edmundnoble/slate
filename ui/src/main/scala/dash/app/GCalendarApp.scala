package dash.app

import cats.data.Xor
import qq.data.{ConcreteFilter, Program}
import qq.macros.QQStager._
import cats.syntax.xor._

object GCalendarApp {
  val program: Program[ConcreteFilter] Xor String =
qq"""
def authHeaders: { Authorization: "Bearer " + googleAuth };
def getEventsOptions: {maxResults: 10, fields: "items(end,location,start,status,summary)",
                       timeMin: nowRFC3339, singleEvents: true, orderBy: "startTime"};
$$calendarIds as httpGet("https://www.googleapis.com/calendar/v3/users/me/calendarList"; {minAccessRole: "writer"}; {}; authHeaders) | .items.[].id in
$$calendarId as $$calendarIds in {
  title: $$calendarId,
  content: httpGet("https://www.googleapis.com/calendar/v3/calendars/" + $$calendarId + "/events";
                   getEventsOptions; {}; authHeaders) |
    [.items.[] | {
      title: .summary + " - " + (.start.dateTime | formatDatetimeFriendly),
      content: .location | orElse("")
    }]
}""".left
}
