package models

// Note: See Election.scala for the election and election state models

import ch.bfh.unicrypt.math.algebra.general.interfaces.Element
import ch.bfh.unicrypt.math.algebra.multiplicative.classes.GStarModSafePrime
import utils.Crypto
import play.api.libs.json._
import play.api.libs.functional.syntax._
import scala.BigInt

/**
 * the group and generator for an election
 */
case class CryptoSettings(group: GStarModSafePrime, generator: Element[_])

/**
 * Serialization (Data Transfer Object) classes
 *
 * This allows us to simulate sending messages over the wire as well
 * as posting to bulletin boards in a neutral format (pure strings/json)
 */
case class EncryptionKeyShareDTO(sigmaProofDTO: SigmaProofDTO, keyShare: String)

case class PartialDecryptionDTO(partialDecryptions: Seq[String], proofDTO: SigmaProofDTO)
case class SigmaProofDTO(commitment: String, challenge: String, response: String)

case class ShuffleResultDTO(shuffleProof: ShuffleProofDTO, votes: Seq[String])
case class PermutationProofDTO(commitment: String, challenge: String, response: String,
  bridgingCommitments: Seq[String], eValues: Seq[String])
case class MixProofDTO(commitment: String, challenge: String, response: String, eValues: Seq[String])
case class ShuffleProofDTO(mixProof: MixProofDTO, permutationProof: PermutationProofDTO, permutationCommitment: String)


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


/** used to signal a validation error when validating votes */
class ValidationException(message: String) extends Exception(message)

trait EncryptionFormatter {
  
  implicit val BigIntegerReads : Reads[BigInt] = 
    (JsPath ).read[String].map{ stringNum => BigInt(stringNum)}
  
  implicit val BigIntegerWrites : Writes[BigInt] = 
    (JsPath ).write[String].contramap{  a : BigInt => a.toString}
  
  implicit val VoteDTOF = Json.format[VoteDTO]
  implicit val ChoiceF = Json.format[Choice]
  implicit val PopkF = Json.format[Popk]
  implicit val EncryptedVoteF = Json.format[EncryptedVote]
}

/** json vote submitted to the ballot box, when validated becomes a Vote */
case class VoteDTO(vote: String, vote_hash: String) extends EncryptionFormatter {
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