package app

import play.api.libs.json._
import shapeless._
import shapeless.ops.nat._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.data.validation.ValidationError
import models._

case class JsCryptoSettings(group: String, generator: String)
case class JsElectionState(id: String, cSettings: JsCryptoSettings)
case class JsCreated(id: String, cSettings: JsCryptoSettings)
case class JsShares(level: Int, shares: (String, String))
case class JsCombined(publicKey: String)
case class JsVotes(votes: List[String], addVoteIndex: Int)
case class JsElection(level: Int, state: JsCreated)
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
    (JsPath \ "cSettings").write[JsCryptoSettings]
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
    (JsPath \ "cSettings").read[JsCryptoSettings]
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
  
  implicit val validateJsCombinedRead: Reads[JsCombined] = 
      (JsPath \ "publicKey").read[String].map{ publicKey => JsCombined(publicKey)}

  implicit val validateJsCombinedWrite: Writes[JsCombined] = 
      (JsPath \ "publicKey").write[String].contramap { a: JsCombined => a.publicKey }
  
  implicit def validateJsVotesReads: Reads[JsVotes] = (
    (JsPath \ "votes").read[List[String]] and
    (JsPath \ "addVoteIndex").read[Int] 
  )(JsVotes.apply _)
  
  implicit val validateJsVotesWrites: Writes[JsVotes] = (
    (JsPath \ "votes").write[List[String]] and
    (JsPath \ "addVoteIndex").write[Int]
  )(unlift(JsVotes.unapply))
}

trait ElectionMachineJSONConverter
  extends ElectionJsonFormatter 
  with BoardJSONFormatter 
{
  def CreatedToPostRequest[W <: Nat: ToInt](input: Election[W, Created]) : PostRequest = {
    val jsElection = JsElection(
      ToInt[W].apply(),
      JsCreated(
        input.state.id, 
        JsCryptoSettings(
          input.state.cSettings.group.getModulus().toString(), 
          input.state.cSettings.generator.convertToString()
        )
    ))
    /*val b64 = new Base64Message(Json.toJson(jsElection))
    // for some reason Fiware doesn't like the '=' character on a String (or \")
    val message = b64.toString().replace('=', '.')*/
    val message = Json.stringify(Json.toJson(jsElection))
    println("GG Post message: " + message)
    PostRequest(message, UserAttributes("election", "create", None, None))
  }
  
  def SharesToPostRequest[W <: Nat: ToInt, T <: Nat: ToInt](input : Election[W, Shares[T]]) : PostRequest = {
    val t = ToInt[T].apply()
    val list = input.state.shares.unsized
    val shares = if(list.length > 0) {
      list.last
    } else {
      ("", "")
    }
    val jsShares = JsShares(t, shares)
    val jsMessage = JsMessage("Shares", Json.toJson(jsShares))
    val message = Json.stringify(Json.toJson(jsMessage))
    println("GG SharesToPostRequest: " + message)
    PostRequest(message, UserAttributes("election", input.state.uid, None, None))
  }
  
  def CombinedToPostRequest[W <: Nat : ToInt](election: Election[W, Combined]) : PostRequest = {
    val jsCombined = JsCombined(election.state.publicKey)
    val jsMessage = JsMessage("Combined", Json.toJson(jsCombined))
    val message = Json.stringify(Json.toJson(jsMessage))
    println("GG CombinedToPostRequest: " + message)
    PostRequest(message, UserAttributes("election", election.state.uid, None, None))
  }
  
  def VotesToPostRequest[W <: Nat : ToInt](election: Election[W, Votes], votes: List[String]) : PostRequest = {
    val jsVotes = JsVotes(votes, election.state.addVoteIndex)
    val jsMessage = JsMessage("Votes", Json.toJson(jsVotes))
    val message = Json.stringify(Json.toJson(jsMessage))
    println("GG VotesToPostRequest: " + message)
    PostRequest(message, UserAttributes("election", election.state.uid, None, None))
  }
}