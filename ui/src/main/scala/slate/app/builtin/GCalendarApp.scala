package slate
package app
package builtin

import qq.cc.{InterpretedFilter, QQInterpreterRuntime}
import qq.macros.stager._

object GCalendarApp {
  val program: InterpretedFilter Either String =
    Left(
      QQStager(QQInterpreterRuntime, SlatePrelude,
        """
def authHeaders: { Authorization: "Bearer " + googleAuth };
def getEventsOptions: {maxResults: 10, fields: "items(end,location,start,status,summary)",
                       timeMin: nowRFC3339, singleEvents: true, orderBy: "startTime"};

httpGet("https://www.googleapis.com/calendar/v3/users/me/calendarList"; {minAccessRole: "writer"}; {}; authHeaders) | .items.[].id |
{
  title: .,
  content: httpGet("https://www.googleapis.com/calendar/v3/calendars/" + . + "/events";
                   getEventsOptions; {}; authHeaders) |
    [.items.[] | {
      title: .summary + " - " + (.start.dateTime | formatDatetimeFriendly),
      content: .location | orElse("")
    }]
}"""
      )
    )
}
