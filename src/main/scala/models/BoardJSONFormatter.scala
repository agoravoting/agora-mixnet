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
  implicit val subscribeRequestReads: Reads[SubscribeRequest] = (
      (JsPath \ "section").read[String] and
      (JsPath \ "group").read[String] and
      (JsPath \ "reference").read[String]
  )(SubscribeRequest.apply _)
  
  implicit val dsaPublicKeyStringReads: Reads[DSAPublicKeyString] = (
      (JsPath \ "y").read[String] and
      (JsPath \ "p").read[String] and
      (JsPath \ "q").read[String] and
      (JsPath \ "g").read[String] 
  )(DSAPublicKeyString.apply _)
  
  implicit val signatureElementsReads: Reads[SignatureElements] = (
      (JsPath \ "first").read[String] and
      (JsPath \ "second").read[String] and
      (JsPath \ "zmod").read[String] 
  )(SignatureElements.apply _)
  
  implicit val signatureStringReads: Reads[SignatureString] = (
      (JsPath \ "signerPK").read[DSAPublicKeyString] and
      (JsPath \ "signaturePK").read[String] and
      (JsPath \ "signature").read[SignatureElements]
  )(SignatureString.apply _)
  
  implicit val userAttributesReads: Reads[UserAttributes] = (
      (JsPath \ "section").read[String] and
      (JsPath \ "group").read[String] and
      (JsPath \ "pk").readNullable[String] and
      (JsPath \ "signature").readNullable[SignatureString] 
  )(UserAttributes.apply _)
  
  implicit val postRequestReads: Reads[PostRequest] = (
      (JsPath \ "message").read[String] and
      (JsPath \ "user_attributes").read[UserAttributes]
  )(PostRequest.apply _)
  
  implicit val boardAttributesReads: Reads[BoardAttributes] = (
      (JsPath \ "index").read[String] and
      (JsPath \ "timestamp").read[String] and
      (JsPath \ "hash").read[String] and
      (JsPath \ "signature").readNullable[SignatureString] 
  )(BoardAttributes.apply _)
  
  implicit val postReads: Reads[Post] = (
      (JsPath \ "message").read[String] and
      (JsPath \ "user_attributes").read[UserAttributes] and
      (JsPath \ "board_attributes").read[BoardAttributes]
  )(Post.apply _)
  
  implicit val getRequestReads: Reads[GetRequest] = (
      (JsPath \ "section").read[String] and
      (JsPath \ "group").read[String] and
      (JsPath \ "index").read[String]
  )(GetRequest.apply _)

  implicit val dsaPublicKeyStringWrites: Writes[DSAPublicKeyString] = (
      (JsPath \ "y").write[String] and
      (JsPath \ "p").write[String] and
      (JsPath \ "q").write[String] and
      (JsPath \ "g").write[String]
  )(unlift(DSAPublicKeyString.unapply))
  
  implicit val signatureElementsWrites: Writes[SignatureElements] = (
      (JsPath \ "first").write[String] and
      (JsPath \ "second").write[String] and
      (JsPath \ "zmod").write[String] 
  )(unlift(SignatureElements.unapply))
  
  implicit val signatureStringWrites: Writes[SignatureString] = (
      (JsPath \ "signerPK").write[DSAPublicKeyString] and
      (JsPath \ "signaturePK").write[String] and
      (JsPath \ "signature").write[SignatureElements]
  )(unlift(SignatureString.unapply))
  
  implicit val userAttributesWrites: Writes[UserAttributes] = (
      (JsPath \ "section").write[String] and
      (JsPath \ "group").write[String] and
      (JsPath \ "pk").writeNullable[String] and
      (JsPath \ "signature").writeNullable[SignatureString] 
  )(unlift(UserAttributes.unapply))
  
  implicit val postRequestWrites: Writes[PostRequest] = (
      (JsPath \ "message").write[String] and
      (JsPath \ "user_attributes").write[UserAttributes]
  )(unlift(PostRequest.unapply))
  
  implicit val boardAttributesWrites: Writes[BoardAttributes] = (
      (JsPath \ "index").write[String] and
      (JsPath \ "timestamp").write[String] and
      (JsPath \ "hash").write[String] and
      (JsPath \ "signature").writeNullable[SignatureString] 
  )(unlift(BoardAttributes.unapply))
  
  implicit val postWrites: Writes[Post] = (
      (JsPath \ "message").write[String] and
      (JsPath \ "user_attributes").write[UserAttributes] and
      (JsPath \ "board_attributes").write[BoardAttributes]
  )(unlift(Post.unapply))
  
  implicit val getRequestWrites: Writes[GetRequest] = (
      (JsPath \ "section").write[String] and
      (JsPath \ "group").write[String] and
      (JsPath \ "index").write[String] 
  )(unlift(GetRequest.unapply))
  
  implicit val subscribeRequestWrites: Writes[SubscribeRequest] = (
      (JsPath \ "section").write[String] and
      (JsPath \ "group").write[String] and
      (JsPath \ "reference").write[String]
  )(unlift(SubscribeRequest.unapply))
}