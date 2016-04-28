package app

import play.api.libs.json._
import shapeless._
import shapeless.ops.nat._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.data.validation.ValidationError

case class JsCryptoSettings(group: String, generator: String)
case class JsElectionState(id: String, cSettings: JsCryptoSettings)
case class JsCreated(id: String, cSettings: JsCryptoSettings, uid: String)
case class JsShares(level: Int, shares: (String, String))
case class JsElection(level: Int, state: JsCreated )
case class JsMessage(messageType: String, message: JsValue)

trait ElectionJsonFormatter {
  
  implicit def tuple2Writes[A, B](implicit aWrites: Writes[A], bWrites: Writes[B]): Writes[Tuple2[A, B]] = new Writes[Tuple2[A, B]] {
    def writes(tuple: Tuple2[A, B]) = JsArray(Seq(aWrites.writes(tuple._1), bWrites.writes(tuple._2)))
  }
  
  implicit def tuple2Reads[A, B](implicit aReads: Reads[A], bReads: Reads[B]): Reads[Tuple2[A, B]] = Reads[Tuple2[A, B]] {
    case JsArray(arr) if arr.size == 2 => for {
      a <- aReads.reads(arr(0))
      b <- bReads.reads(arr(1))

    } yield (a, b)
    case _ => JsError(Seq(JsPath() -> Seq(ValidationError("Expected array of two elements"))))
  }
  
  implicit val jsJsSharesWrites: Writes[JsShares] = (
    (JsPath \ "level").write[Int] and
    (JsPath \ "shares").write[Tuple2[String, String]]
  )(unlift(JsShares.unapply))
  
  implicit val jsJsMessageWrites: Writes[JsMessage] = (
    (JsPath \ "messageType").write[String] and
    (JsPath \ "message").write[JsValue]
  )(unlift(JsMessage.unapply))
  
  implicit val jsCryptoSettingsWrites: Writes[JsCryptoSettings] = (
    (JsPath \ "group").write[String] and
    (JsPath \ "generator").write[String]
  )(unlift(JsCryptoSettings.unapply))
  
  implicit val jsJsCreatedWrites: Writes[JsCreated] = (
    (JsPath \ "id").write[String] and
    (JsPath \ "cSettings").write[JsCryptoSettings] and
    (JsPath \ "uid").write[String]
  )(unlift(JsCreated.unapply))
  
  implicit val jsElectionStateWrites: Writes[JsElectionState] = (
    (JsPath \ "id").write[String] and
    (JsPath \ "cSettings").write[JsCryptoSettings]
  )(unlift(JsElectionState.unapply))
  
  implicit def jsJsElectionWrites: Writes[JsElection] = (
    (JsPath \ "level").write[Int] and
    (JsPath \ "state").write[JsCreated]
  )(unlift(JsElection.unapply))
  
  implicit val jsCryptoSettingsReads: Reads[JsCryptoSettings] = (
    (JsPath \ "group").read[String] and
    (JsPath \ "generator").read[String]
  )(JsCryptoSettings.apply _)
  
  implicit val jsElectionStateReads: Reads[JsElectionState] = (
    (JsPath \ "id").read[String] and
    (JsPath \ "cSettings").read[JsCryptoSettings]
  )(JsElectionState.apply _)
  
  
  implicit val jsJsCreatedReads: Reads[JsCreated] = (
    (JsPath \ "id").read[String] and
    (JsPath \ "cSettings").read[JsCryptoSettings] and
    (JsPath \ "uid").read[String]
  )(JsCreated.apply _)
  
  implicit def jsJsElectionReads: Reads[JsElection] = (
    (JsPath \ "level").read[Int] and
    (JsPath \ "state").read[JsCreated] 
  )(JsElection.apply _)
  
  implicit def jsJsMessageReads: Reads[JsMessage] = (
    (JsPath \ "messageType").read[String] and
    (JsPath \ "message").read[JsValue] 
  )(JsMessage.apply _)
  
  implicit def jsJsSharesReads: Reads[JsShares] = (
    (JsPath \ "level").read[Int] and
    (JsPath \ "shares").read[Tuple2[String, String]] 
  )(JsShares.apply _)
}

trait ElectionMachineJSONConverter extends ElectionJsonFormatter {
  
  def CreatedToJS[W <: Nat: ToInt](input: Election[W, Created]) : JsValue = {
    val jsElection = JsElection(
      ToInt[W].apply(),
      JsCreated(
        input.state.id, 
        JsCryptoSettings(
          input.state.cSettings.group.getModulus().toString(), 
          input.state.cSettings.generator.convertToString()
        ),
        input.state.uid
    ))
    Json.toJson(jsElection)
  }
}