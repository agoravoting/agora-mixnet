package models

import play.api.libs.json._
import java.sql.Timestamp
import java.util.Date

trait ElectionDTOFormatter {
  val dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS")

  implicit val formatTimestamp = new Format[Timestamp] {
    def writes(ts: Timestamp): JsValue = {
      JsString(dateFormat.format(ts))
    }
    def reads(ts: JsValue): JsResult[Timestamp] = {
      try {
        val date = dateFormat.parse(ts.as[String])
        JsSuccess(new Timestamp(date.getTime))
      } catch {
        case e: IllegalArgumentException => JsError("Unable to parse timestamp")
      }
    }
  }
  
  implicit val electionExtraFormatter = Json.format[ElectionExtra]
  implicit val urlFormatter = Json.format[Url]
  implicit val answerFormatter = Json.format[Answer]
  implicit val questionFormatter = Json.format[Question]
  implicit val electionPresentationFormatter = Json.format[ElectionPresentation]
  implicit val electionConfigFormatter = Json.format[ElectionConfig]
  implicit val electionDTOFormatter = Json.format[ElectionDTO]
}