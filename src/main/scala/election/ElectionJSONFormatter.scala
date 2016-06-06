package election

import play.api.libs.json._
import shapeless._
import shapeless.ops.nat._
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
case class JsDecryptions(level: Int, decryption: PartialDecryptionDTO)
case class JsDecrypted(decrypted: Seq[String])
case class JsElection(level: Int, state: JsCreated, dto: ElectionDTO)
case class JsMessage(messageType: String, message: JsValue)

trait ElectionJsonFormatter extends ElectionDTOFormatter {
      
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
  
  implicit val JsSharesF = Json.format[JsShares]
  implicit val JsMessageF = Json.format[JsMessage]
  implicit val JsCryptoSettingsF = Json.format[JsCryptoSettings]
  implicit val JsCreatedF = Json.format[JsCreated]
  implicit val JsElectionStateF = Json.format[JsElectionState]
  implicit val JsElectionF = Json.format[JsElection]
  implicit val JsVotesF = Json.format[JsVotes]
  implicit val JsVotesStoppedF = Json.format[JsVotesStopped]
  implicit val SigmaProofDTOF = Json.format[SigmaProofDTO]
  implicit val PartialDecryptionDTOF = Json.format[PartialDecryptionDTO]
  implicit val PermutationProofDTOF = Json.format[PermutationProofDTO]
  implicit val MixProofDTOF = Json.format[MixProofDTO]
  implicit val ShuffleProofDTOF = Json.format[ShuffleProofDTO]
  implicit val ShuffleResultDTOF = Json.format[ShuffleResultDTO]
  implicit val JsMixingF = Json.format[JsMixing]
  implicit val JsDecryptionsF = Json.format[JsDecryptions]
  implicit val JsDecryptedF = Json.format[JsDecrypted]
  implicit val JsCombinedF = Json.format[JsCombined]
}

trait ElectionMachineJSONConverter
  extends ElectionJsonFormatter 
  with BoardJSONFormatter 
{
  def CreatedToPostRequest[W <: Nat: ToInt](input: Election[W, Created]) : PostRequest = {
    val jsElection = JsElection(
      ToInt[W].apply(),
      JsCreated(
        input.state.uid, 
        JsCryptoSettings(
          input.state.cSettings.group.getModulus().toString(), 
          input.state.cSettings.generator.convertToString()
        )
       ),
       input.state.dto
    )
    val message = Json.stringify(Json.toJson(jsElection))
    println("GG Post message with uid " + input.state.uid)
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
    println("GG VotesToPostRequest: " )
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
    println("GG StartMixingToPostRequest: ")
    PostRequest(message, UserAttributes("election", election.state.uid, None, None))
  }
  
  def MixingToPostRequest[W <: Nat : ToInt, T <: Nat : ToInt](election: Election[W, Mixing[T]], mixes: ShuffleResultDTO) : PostRequest = {
    val jsMixing = JsMixing(ToInt[T].apply(), mixes)
    val jsMessage = JsMessage("Mixing", Json.toJson(jsMixing))
    val message = Json.stringify(Json.toJson(jsMessage))
    println("GG MixingToPostRequest: ")
    PostRequest(message, UserAttributes("election", election.state.uid, None, None))
  }
  
  def MixedToPostRequest[W <: Nat : ToInt](election: Election[W, Mixed]) : PostRequest = {
    val jsMessage = JsMessage("Mixed", JsNull)
    val message = Json.stringify(Json.toJson(jsMessage))
    println("GG MixedToPostRequest: ")
    PostRequest(message, UserAttributes("election", election.state.uid, None, None))
  }
  
  def StartDecryptionsToPostRequest[W <: Nat : ToInt](election: Election[W, Decryptions[_0]]) : PostRequest = {
    val jsMessage = JsMessage("StartDecryptions", JsNull)
    val message = Json.stringify(Json.toJson(jsMessage))
    println("GG StartDecryptionsToPostRequest: ")
    PostRequest(message, UserAttributes("election", election.state.uid, None, None))
  }
  
  def AddDecryptionToPostRequest[W <: Nat : ToInt, T <: Nat : ToInt](election: Election[W, Decryptions[T]], decryption: PartialDecryptionDTO) : PostRequest = {
    val jsDecryptions = JsDecryptions(ToInt[T].apply(), decryption)
    val jsMessage = JsMessage("Decryptions", Json.toJson(jsDecryptions))
    val message = Json.stringify(Json.toJson(jsMessage))
    println("GG AddDecryptionToPostRequest: ")
    PostRequest(message, UserAttributes("election", election.state.uid, None, None))
  }
  
  def DecryptedToPostRequest[W <: Nat : ToInt](election: Election[W, Decrypted]) : PostRequest = {
    val jsDecrypted = JsDecrypted(election.state.decrypted)
    val jsMessage = JsMessage("Decrypted", Json.toJson(jsDecrypted))
    val message = Json.stringify(Json.toJson(jsMessage))
    println("GG DecryptedToPostRequest: ")
    PostRequest(message, UserAttributes("election", election.state.uid, None, None))
  }
}