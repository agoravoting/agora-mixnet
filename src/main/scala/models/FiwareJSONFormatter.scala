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

trait FiwareJSONFormatter {
  
  // Subscribe success validators
  
  implicit val validateSubscribeResponseRead: Reads[SubscribeResponse] = (
      (JsPath \ "subscriptionId").read[String] and
      (JsPath \ "duration").read[String] and
      (JsPath \ "throttling").read[String]
  )(SubscribeResponse.apply _)
  
  implicit val validateSuccessfulSubscribeRead: Reads[SuccessfulSubscribe] = 
      (JsPath \ "subscribeResponse").read[SubscribeResponse].map{ errorCode => SuccessfulSubscribe(errorCode)}
  
  // Get success validators
  
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
  
  implicit val validateGetStatusCodeRead: Reads[StatusCode] = (
      (JsPath \ "code").read[String] and
      (JsPath \ "reasonPhrase").read[String]
  )(StatusCode.apply _)
  
  implicit val validateGetContextResponseRead: Reads[ContextResponse] = (
      (JsPath \ "contextElement").read[ContextElement] and
      (JsPath \ "statusCode").read[StatusCode]
  )(ContextResponse.apply _)
  
  implicit val validateSuccessfulGetPostRead: Reads[SuccessfulGetPost] = 
      (JsPath \ "contextResponses").read[Seq[ContextResponse]].map{ contextResponses => SuccessfulGetPost(contextResponses)}
  
  // Get failure validators
  
  implicit val validateGetErrorCodeRead: Reads[GetErrorCode] = (
      (JsPath \ "code").read[String] and
      (JsPath \ "reasonPhrase").read[String]
  )(GetErrorCode.apply _)
    
  // see http://stackoverflow.com/questions/14754092/how-to-turn-json-to-case-class-when-case-class-has-only-one-field
  implicit val validateFailedGetRead: Reads[FailedGetPost] = 
      (JsPath \ "errorCode").read[GetErrorCode].map{ errorCode => FailedGetPost(errorCode)}
    
  implicit val validateAccumulateRequestRead: Reads[AccumulateRequest] = (
      (JsPath \ "subscriptionId").read[String] and
      (JsPath \ "originator").read[String] and
      (JsPath \ "contextResponses").read[Seq[ContextResponse]] (minLength[Seq[ContextResponse]](1))
  )(AccumulateRequest.apply _)
  
  implicit val validateSubscribeResponseWrite: Writes[SubscribeResponse] = (
      (JsPath \ "subscriptionId").write[String] and
      (JsPath \ "duration").write[String] and
      (JsPath \ "throttling").write[String]
  )(unlift(SubscribeResponse.unapply))
  
  // see http://stackoverflow.com/questions/14754092/how-to-turn-json-to-case-class-when-case-class-has-only-one-field
  implicit val validateSuccessfulSubscribeWrite: Writes[SuccessfulSubscribe] = 
      (JsPath \ "subscribeResponse").write[SubscribeResponse].contramap { a: SuccessfulSubscribe => a.subscribeResponse }
  
  implicit val validateAttributeWrite: Writes[Attribute] = (
      (JsPath \ "name").write[String] and
      (JsPath \ "type").write[String] and
      (JsPath \ "value").write[JsValue]
  )(unlift(Attribute.unapply))
  
  implicit val validateContextElementWrite: Writes[ContextElement] = (
      (JsPath \ "id").write[String] and
      (JsPath \ "isPattern").write[String] and
      (JsPath \ "type").write[String] and
      (JsPath \ "attributes").write[Seq[Attribute]]
  )(unlift(ContextElement.unapply))
  
  implicit val validateStatusCodeWrite: Writes[StatusCode] = (
      (JsPath \ "code").write[String] and
      (JsPath \ "reasonPhrase").write[String]
  )(unlift(StatusCode.unapply))
  
  implicit val validateContextResponseWrite: Writes[ContextResponse] = (
      (JsPath \ "contextElement").write[ContextElement] and
      (JsPath \ "statusCode").write[StatusCode]
  )(unlift(ContextResponse.unapply))
  
  implicit val validateAccumulateRequestWrite: Writes[AccumulateRequest] = (
      (JsPath \ "subscriptionId").write[String] and
      (JsPath \ "originator").write[String] and
      (JsPath \ "contextResponses").write[Seq[ContextResponse]]
  )(unlift(AccumulateRequest.unapply))
    
  implicit val validateSuccessfulGetPostWrites: Writes[SuccessfulGetPost] = 
      (JsPath \ "contextResponses").write[Seq[ContextResponse]].contramap((f: SuccessfulGetPost) => f.contextResponses)
  
}