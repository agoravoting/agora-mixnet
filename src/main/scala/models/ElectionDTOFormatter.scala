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