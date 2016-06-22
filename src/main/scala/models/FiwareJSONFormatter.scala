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
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

// Classes used for validating and parsing JSON

case class Attribute(name: String, _type: String, value: JsValue)
case class ContextElement(id: String, isPattern: String, _type: String, attributes: Seq[Attribute])
case class StatusCode(code: String, reasonPhrase: String)
case class ContextResponse(contextElement: ContextElement, statusCode: StatusCode)
case class SuccessfulGetPost(contextResponses: Seq[ContextResponse])

case class SubscribeResponse(subscriptionId: String, duration: String, throttling: String)
case class SuccessfulSubscribe(subscribeResponse: SubscribeResponse)

case class GetErrorCode(code: String, reasonPhrase: String)
case class FailedGetPost(errorCode: GetErrorCode)

case class AccumulateRequest(subscriptionId: String, originator: String, contextResponses: Seq[ContextResponse])
case class UnsubscribeRequest(subscriptionId: String, reference: String)

trait FiwareJSONFormatter {
  
  implicit val validateAttributeWrite: Writes[Attribute] = (
      (JsPath \ "name").write[String] and
      (JsPath \ "type").write[String] and
      (JsPath \ "value").write[JsValue]
  )(unlift(Attribute.unapply))
  
  implicit val validateGetAttributeRead: Reads[Attribute] = (
      (JsPath \ "name").read[String] and
      (JsPath \ "type").read[String] and
      (JsPath \ "value").read[JsValue]
  )(Attribute.apply _)  
    
  implicit val validateGetContextElementRead: Reads[ContextElement] = (
      (JsPath \ "id").read[String] and
      (JsPath \ "isPattern").read[String] and
      (JsPath \ "type").read[String] and
      (JsPath \ "attributes").read[Seq[Attribute]](minLength[Seq[Attribute]](1) keepAnd maxLength[Seq[Attribute]](1))
  )(ContextElement.apply _)
  
  implicit val validateContextElementWrite: Writes[ContextElement] = (
      (JsPath \ "id").write[String] and
      (JsPath \ "isPattern").write[String] and
      (JsPath \ "type").write[String] and
      (JsPath \ "attributes").write[Seq[Attribute]]
  )(unlift(ContextElement.unapply))
  
  implicit val SubscribeResponseF = Json.format[SubscribeResponse]
  implicit val SuccessfulSubscribeF = Json.format[SuccessfulSubscribe]
  implicit val StatusCodeF = Json.format[StatusCode]
  implicit val ContextResponseF = Json.format[ContextResponse]
  implicit val SuccessfulGetPostF = Json.format[SuccessfulGetPost]
  implicit val GetErrorCodeF = Json.format[GetErrorCode]
  implicit val FailedGetPostF = Json.format[FailedGetPost]
  implicit val AccumulateRequestF = Json.format[AccumulateRequest]
  implicit val UnsubscribeRequestF = Json.format[UnsubscribeRequest]
}