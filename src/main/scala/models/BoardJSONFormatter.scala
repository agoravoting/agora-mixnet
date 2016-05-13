package models

import javax.inject._
import play.api.libs.json._
import play.api.libs.functional.syntax._

// Classes used for reading and writing JSON structures of a Post

case class DSAPublicKeyString(y: String, p: String, q: String, g: String)
case class SignatureElements(first: String, second: String, zmod: String)
case class SignatureString(signerPK: DSAPublicKeyString, signaturePK: String, signature: SignatureElements)
case class UserAttributes(section: String, group: String, pk: Option[String] = None, signature: Option[SignatureString] = None)
case class BoardAttributes(index: String, timestamp: String, hash: String, signature: Option[SignatureString] = None)
case class PostRequest(message: String, user_attributes: UserAttributes)
case class Post(message: String, user_attributes: UserAttributes, board_attributes: BoardAttributes)
case class GetRequest(section: String, group: String, index: String)
case class SubscribeRequest(section: String, group: String, reference: String)

// This trait enables easily reading a Json into a Post
trait BoardJSONFormatter {
  implicit val SubscribeRequestF = Json.format[SubscribeRequest]
  implicit val DSAPublicKeyStringF = Json.format[DSAPublicKeyString]
  implicit val SignatureElementsF = Json.format[SignatureElements]
  implicit val SignatureStringF = Json.format[SignatureString]
  implicit val UserAttributesF = Json.format[UserAttributes]
  implicit val PostRequestF = Json.format[PostRequest]
  implicit val BoardAttributesF = Json.format[BoardAttributes]
  implicit val PostF = Json.format[Post]
  implicit val GetRequestF = Json.format[GetRequest]
}