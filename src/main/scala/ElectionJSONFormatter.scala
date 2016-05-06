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
case class JsVotesStopped(lastAddVoteIndex: Int, date: String)
case class JsMixing(level: Int, mixes: ShuffleResultDTO)
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
  
  implicit val validateJsSharesWrites: Writes[JsShares] = (
    (JsPath \ "level").write[Int] and
    (JsPath \ "shares").write[Tuple2[String, String]]
  )(unlift(JsShares.unapply))
  
  implicit val validateJsMessageWrites: Writes[JsMessage] = (
    (JsPath \ "messageType").write[String] and
    (JsPath \ "message").write[JsValue]
  )(unlift(JsMessage.unapply))
  
  implicit val validateCryptoSettingsWrites: Writes[JsCryptoSettings] = (
    (JsPath \ "group").write[String] and
    (JsPath \ "generator").write[String]
  )(unlift(JsCryptoSettings.unapply))
  
  implicit val validateJsCreatedWrites: Writes[JsCreated] = (
    (JsPath \ "id").write[String] and
    (JsPath \ "cSettings").write[JsCryptoSettings]
  )(unlift(JsCreated.unapply))
  
  implicit val validateElectionStateWrites: Writes[JsElectionState] = (
    (JsPath \ "id").write[String] and
    (JsPath \ "cSettings").write[JsCryptoSettings]
  )(unlift(JsElectionState.unapply))
  
  implicit val validateJsElectionWrites: Writes[JsElection] = (
    (JsPath \ "level").write[Int] and
    (JsPath \ "state").write[JsCreated]
  )(unlift(JsElection.unapply))
  
  implicit val validateCryptoSettingsReads: Reads[JsCryptoSettings] = (
    (JsPath \ "group").read[String] and
    (JsPath \ "generator").read[String]
  )(JsCryptoSettings.apply _)
  
  implicit val validateElectionStateReads: Reads[JsElectionState] = (
    (JsPath \ "id").read[String] and
    (JsPath \ "cSettings").read[JsCryptoSettings]
  )(JsElectionState.apply _)
  
  implicit val validateJsCreatedReads: Reads[JsCreated] = (
    (JsPath \ "id").read[String] and
    (JsPath \ "cSettings").read[JsCryptoSettings]
  )(JsCreated.apply _)
  
  implicit val validateJsElectionReads: Reads[JsElection] = (
    (JsPath \ "level").read[Int] and
    (JsPath \ "state").read[JsCreated] 
  )(JsElection.apply _)
  
  implicit val validateJsMessageReads: Reads[JsMessage] = (
    (JsPath \ "messageType").read[String] and
    (JsPath \ "message").read[JsValue] 
  )(JsMessage.apply _)
  
  implicit val validateJsSharesReads: Reads[JsShares] = (
    (JsPath \ "level").read[Int] and
    (JsPath \ "shares").read[Tuple2[String, String]] 
  )(JsShares.apply _)
  
  implicit val validateJsCombinedRead: Reads[JsCombined] = 
      (JsPath \ "publicKey").read[String].map{ publicKey => JsCombined(publicKey)}

  implicit val validateJsCombinedWrite: Writes[JsCombined] = 
      (JsPath \ "publicKey").write[String].contramap { a: JsCombined => a.publicKey }
  
  implicit val validateJsVotesReads: Reads[JsVotes] = (
    (JsPath \ "votes").read[List[String]] and
    (JsPath \ "addVoteIndex").read[Int] 
  )(JsVotes.apply _)
  
  implicit val validateJsVotesWrites: Writes[JsVotes] = (
    (JsPath \ "votes").write[List[String]] and
    (JsPath \ "addVoteIndex").write[Int]
  )(unlift(JsVotes.unapply))
  
  implicit def validateJsVotesStoppedReads: Reads[JsVotesStopped] = (
    (JsPath \ "lastAddVoteIndex").read[Int] and
    (JsPath \ "date").read[String] 
  )(JsVotesStopped.apply _)
  
  implicit val validateJsVotesStoppedWrites: Writes[JsVotesStopped] = (
    (JsPath \ "lastAddVoteIndex").write[Int] and
    (JsPath \ "date").write[String]
  )(unlift(JsVotesStopped.unapply))  
  
  implicit def validateSigmaProofDTOReads: Reads[SigmaProofDTO] = (
    (JsPath \ "commitment").read[String] and
    (JsPath \ "challenge").read[String] and
    (JsPath \ "response").read[String] 
  )(SigmaProofDTO.apply _)
  
  implicit val validateSigmaProofDTOWrites: Writes[SigmaProofDTO] = (
    (JsPath \ "commitment").write[String] and
    (JsPath \ "challenge").write[String] and
    (JsPath \ "response").write[String]
  )(unlift(SigmaProofDTO.unapply))
  
  implicit val validatePartialDecryptionDTOReads: Reads[PartialDecryptionDTO] = (
    (JsPath \ "partialDecryptions").read[Seq[String]] and
    (JsPath \ "proofDTO").read[SigmaProofDTO] 
  )(PartialDecryptionDTO.apply _)
  
  implicit val validatePartialDecryptionDTOWrites: Writes[PartialDecryptionDTO] = (
    (JsPath \ "partialDecryptions").write[Seq[String]] and
    (JsPath \ "proofDTO").write[SigmaProofDTO]
  )(unlift(PartialDecryptionDTO.unapply))
  
  implicit val validatePermutationProofDTOReads: Reads[PermutationProofDTO] = (
    (JsPath \ "commitment").read[String] and
    (JsPath \ "challenge").read[String] and
    (JsPath \ "response").read[String] and
    (JsPath \ "bridgingCommitments").read[Seq[String]] and
    (JsPath \ "eValues").read[Seq[String]] 
  )(PermutationProofDTO.apply _)
  
  implicit val validatePermutationProofDTOWrites: Writes[PermutationProofDTO] = (
    (JsPath \ "commitment").write[String] and
    (JsPath \ "challenge").write[String] and
    (JsPath \ "response").write[String] and
    (JsPath \ "bridgingCommitments").write[Seq[String]] and
    (JsPath \ "eValues").write[Seq[String]]
  )(unlift(PermutationProofDTO.unapply))
  
  implicit def validateMixProofDTOReads: Reads[MixProofDTO] = (
    (JsPath \ "commitment").read[String] and
    (JsPath \ "challenge").read[String] and
    (JsPath \ "response").read[String] and
    (JsPath \ "eValues").read[Seq[String]] 
  )(MixProofDTO.apply _)
  
  implicit val validateMixProofDTOWrites: Writes[MixProofDTO] = (
    (JsPath \ "commitment").write[String] and
    (JsPath \ "challenge").write[String] and
    (JsPath \ "response").write[String] and
    (JsPath \ "eValues").write[Seq[String]]
  )(unlift(MixProofDTO.unapply))  
  
  implicit val validateShuffleProofDTOReads: Reads[ShuffleProofDTO] = (
    (JsPath \ "mixProof").read[MixProofDTO] and
    (JsPath \ "permutationProof").read[PermutationProofDTO] and
    (JsPath \ "permutationCommitment").read[String] 
  )(ShuffleProofDTO.apply _)
  
  implicit val validateShuffleProofDTOWrites: Writes[ShuffleProofDTO] = (
    (JsPath \ "mixProof").write[MixProofDTO] and
    (JsPath \ "permutationProof").write[PermutationProofDTO] and
    (JsPath \ "permutationCommitment").write[String]
  )(unlift(ShuffleProofDTO.unapply))
  
  implicit val validateShuffleResultDTOReads: Reads[ShuffleResultDTO] = (
    (JsPath \ "shuffleProof").read[ShuffleProofDTO] and
    (JsPath \ "addVoteIndex").read[Seq[String]] 
  )(ShuffleResultDTO.apply _)
  
  implicit val validateShuffleResultDTOWrites: Writes[ShuffleResultDTO] = (
    (JsPath \ "shuffleProof").write[ShuffleProofDTO] and
    (JsPath \ "addVoteIndex").write[Seq[String]]
  )(unlift(ShuffleResultDTO.unapply))
  
  
  implicit def validateJsMixingReads: Reads[JsMixing] = (
    (JsPath \ "level").read[Int] and
    (JsPath \ "mixes").read[ShuffleResultDTO] 
  )(JsMixing.apply _)
  
  implicit val validateJsMixingWrites: Writes[JsMixing] = (
    (JsPath \ "level").write[Int] and
    (JsPath \ "mixes").write[ShuffleResultDTO]
  )(unlift(JsMixing.unapply))
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
  
  def VotesStoppedToPostRequest[W <: Nat : ToInt](election: Election[W, VotesStopped]) : PostRequest = {
    val jsVotesStopped = JsVotesStopped(election.state.prev.addVoteIndex, election.state.date.toString)
    val jsMessage = JsMessage("VotesStopped", Json.toJson(jsVotesStopped))
    val message = Json.stringify(Json.toJson(jsMessage))
    println("GG VotesStoppedToPostRequest: " + message)
    PostRequest(message, UserAttributes("election", election.state.uid, None, None))
  }
  
  def StartMixingToPostRequest[W <: Nat : ToInt](election: Election[W, Mixing[_0]]) : PostRequest = {
    val jsMessage = JsMessage("StartMixing", JsNull)
    val message = Json.stringify(Json.toJson(jsMessage))
    println("GG StartMixingToPostRequest: " + message)
    PostRequest(message, UserAttributes("election", election.state.uid, None, None))
  }
  
  def MixingToPostRequest[W <: Nat : ToInt, T <: Nat : ToInt](election: Election[W, Mixing[T]], mixes: ShuffleResultDTO) : PostRequest = {
    val jsMixing = JsMixing(ToInt[T].apply(), mixes)
    val jsMessage = JsMessage("Mixing", Json.toJson(jsMixing))
    val message = Json.stringify(Json.toJson(jsMessage))
    println("GG MixingToPostRequest: " + message)
    PostRequest(message, UserAttributes("election", election.state.uid, None, None))
  }
  
  def MixedToPostRequest[W <: Nat : ToInt](election: Election[W, Mixed]) : PostRequest = {
    val jsMessage = JsMessage("Mixed", JsNull)
    val message = Json.stringify(Json.toJson(jsMessage))
    println("GG MixedToPostRequest: " + message)
    PostRequest(message, UserAttributes("election", election.state.uid, None, None))
  }
}