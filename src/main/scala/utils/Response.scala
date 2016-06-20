/**
 * This file is part of agora-mixnet.
 * Copyright (C) 2015-2016  Agora Voting SL <agora@agoravoting.com>

 * agora-mixnet is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License.

 * agora-mixnet is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.

 * You should have received a copy of the GNU Affero General Public License
 * along with agora-mixnet.  If not, see <http://www.gnu.org/licenses/>.
**/

package utils

import play.api.libs.json._
import models._

import java.sql.Timestamp
import java.util.Date

/**
  * Utilities for json messaging
  *
  */
trait Response extends ElectionDTOFormatter {
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
  
  implicit val errorFormatter = Json.format[Error]

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