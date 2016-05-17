package utils

import play.api.libs.json._
import models._

import java.sql.Timestamp
import java.util.Date

/**
  * Utilities for json messaging
  *
  */
trait Response {
  case class Response[T: Format](payload: T)
  case class Error(error: String, code: Int)

  object ErrorCodes {
    val MISSING_AUTH = 1
    val EO_ERROR = 2
    val PK_ERROR = 3
    val TALLY_ERROR = 4
    val GENERAL_ERROR = 5
    val NO_ELECTION = 6
    val NO_PKS = 7
  }

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
  
  implicit val errorFormatter = Json.format[Error]
  implicit val electionExtraFormatter = Json.format[ElectionExtra]
  implicit val urlFormatter = Json.format[Url]
  implicit val answerFormatter = Json.format[Answer]
  implicit val questionFormatter = Json.format[Question]
  implicit val electionPresentationFormatter = Json.format[ElectionPresentation]
  implicit val electionConfigFormatter = Json.format[ElectionConfig]
  implicit val electionDTOFormatter = Json.format[ElectionDTO]

  /** need to manually write reads/writes for generic types */
  implicit def responseReads[T: Format]: Reads[Response[T]] = new Reads[Response[T]] {
    def reads(json: JsValue): JsResult[Response[T]] = JsSuccess(new Response[T] (
       (json \ "payload").as[T]
    ))
  }

  /** need to manually write reads/writes for generic types */
  implicit def responseWrites[T: Format]: Writes[Response[T]] = new Writes[Response[T]] {
    def writes(response: Response[T]) = JsObject(Seq(
        "date" -> JsString(new java.sql.Timestamp(System.currentTimeMillis()).toString),
        "payload" -> Json.toJson(response.payload)
    ))
  }

  def error(error: String, code: Int = ErrorCodes.GENERAL_ERROR) = {
    Json.toJson(Response(Error(error, code)))
  }

  def response[T: Format](payload: T) = {
    Json.toJson(Response(payload))
  }
}