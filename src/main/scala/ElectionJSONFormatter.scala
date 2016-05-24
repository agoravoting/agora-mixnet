package app

import play.api.libs.json._
import shapeless._
import shapeless.ops.nat._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.data.validation.ValidationError
import models._
import utils.Crypto

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
case class JsElection(level: Int, state: JsCreated)
case class JsMessage(messageType: String, message: JsValue)


/** el gamal public key */
case class PublicKey(q: BigInt, p: BigInt, y:BigInt, g: BigInt)


/** the ciphertext present in a json vote (VoteDTO), including proofs of plaintext knowledge */
case class EncryptedVote(choices: Seq[Choice], issue_date: String, proofs: Seq[Popk]) {

  /** ciphertext validation: choice is quadratic residue and validation of proof of plaintext knowledge */
  def validate(pks: Seq[PublicKey], checkResidues: Boolean) = {

    if(checkResidues) {
      choices.zipWithIndex.foreach { case (choice, index) =>
        choice.validate(pks(index))
      }
    }

    checkPopk(pks)
  }

  /** validates proof of plaintext knowledge, schnorr protocol */
  def checkPopk(pks: Seq[PublicKey]) = {

    proofs.zipWithIndex.foreach { case (proof, index) =>
      val choice = choices(index)

      val toHash = s"${choice.alpha.toString}/${proof.commitment.toString}"
      val hashed = Crypto.sha256(toHash)
      val expected = BigInt(hashed, 16)

      if (!proof.challenge.equals(expected)) {
        throw new ValidationException("Popk hash mismatch")
      }

      val pk = pks(index)

      val first = pk.g.modPow(proof.response, pk.p)
      val second = (choice.alpha.modPow(proof.challenge, pk.p) * proof.commitment).mod(pk.p)

      if(!first.equals(second)) {
        throw new ValidationException("Failed verifying popk")
      }
    }
  }
}
/** the el-gamal ciphertext itself */
case class Choice(alpha: BigInt, beta: BigInt) {

  /** checks that both alpha and beta are quadratic residues in p */
  def validate(pk: PublicKey) = {

    if(!Crypto.quadraticResidue(alpha, pk.p)) throw new ValidationException("Alpha quadratic non-residue")
    if(!Crypto.quadraticResidue(beta, pk.p)) throw new ValidationException("Beta quadratic non-residue")
  }
}

/** proof of plaintext knowledge, according to schnorr protocol*/
case class Popk(challenge: BigInt, commitment: BigInt, response: BigInt)

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
  
  implicit val BigIntegerReads : Reads[BigInt] = 
    (JsPath ).read[String].map{ stringNum => BigInt(stringNum)}
  
  implicit val BigIntegerWrites : Writes[BigInt] = 
    (JsPath ).write[String].contramap{  a : BigInt => a.toString}
  
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
  implicit val VoteDTOF = Json.format[VoteDTO]
  implicit val ChoiceF = Json.format[Choice]
  implicit val PopkF = Json.format[Popk]
  implicit val EncryptedVoteF = Json.format[EncryptedVote]
}

/** json vote submitted to the ballot box, when validated becomes a Vote */
case class VoteDTO(vote: String, vote_hash: String) extends ElectionJsonFormatter {
  def validate(pks: PublicKey, checkResidues: Boolean, electionId: Long, voterId: String) : String = {
    val json = Json.parse(vote)
    val encryptedValue = json.validate[EncryptedVote]
    
    encryptedValue.fold (
      errors => throw new ValidationException(s"Error parsing vote json: $errors"),
      encrypted => {

        encrypted.validate(Seq(pks), checkResidues)

        val hashed = Crypto.sha256(vote)

        if(hashed != vote_hash) throw new ValidationException("Hash mismatch")
        
        "[\"" + encrypted.choices(0).alpha.toString + "\"|\"" + encrypted.choices(0).beta.toString + "\"]"
      }
    )
  }
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
    println("GG Post message: ")
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

/** used to signal a validation error when validating votes */
class ValidationException(message: String) extends Exception(message)