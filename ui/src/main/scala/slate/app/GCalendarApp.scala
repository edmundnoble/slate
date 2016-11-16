package slate
package app


import qq.data.{FilterAST, Program}
import qq.macros.QQStager._

object GCalendarApp {
  val program: Program[FilterAST] Either String =
    Left(
      qq"""
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
}
