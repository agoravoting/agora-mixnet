import shapeless._
import nat._
import syntax.sized._
import ops.nat._
import LT._
import com.github.nscala_time.time.Imports._
import com.typesafe.config.ConfigFactory

import ch.bfh.unicrypt.math.algebra.multiplicative.classes.GStarModSafePrime
import ch.bfh.unicrypt.crypto.schemes.encryption.classes.ElGamalEncryptionScheme
import ch.bfh.unicrypt.math.algebra.general.interfaces.Element
import ch.bfh.unicrypt.math.algebra.general.classes.Pair
import ch.bfh.unicrypt.math.algebra.general.classes.Tuple
import ch.bfh.unicrypt.crypto.encoder.classes.ZModPrimeToGStarModSafePrime
import ch.bfh.unicrypt.crypto.encoder.interfaces.Encoder
import ch.bfh.unicrypt.math.algebra.general.abstracts.AbstractSet
import ch.bfh.unicrypt.math.algebra.general.classes.ProductSet
import mpservice.MPBridgeS
import mpservice.MPBridge

import scala.collection.JavaConversions._
import scala.concurrent.Future

/**
 * An election is a typed, purely function state machine with an immutable history
 *
 * The parameters are privacy level, W, and current state, S
 *
 */
class Election[+W <: Nat, +S <: ElectionState] (val state: S) {
  override def toString() = s"election ${state.id}, ${state.toString}"
}

/**
 * These types represent the state of the election and associated information
 *
 */
case class Created(override val id: String, override val cSettings: CryptoSettings, val uid: String) extends ElectionState(id, cSettings)
case class Shares[T <: Nat](val shares: Sized[List[(String, String)], T], prev: ElectionState) extends ElectionStateShares(prev, shares.toList) with HasHistory
case class Combined(override val publicKey: String, prev: ElectionStateShares) extends ElectionStatePk(prev, publicKey) with HasHistory
case class Votes(votes: List[String], prev: ElectionStatePk) extends ElectionStatePk(prev, prev.publicKey) with HasHistory
case class VotesStopped(prev: Votes, date: DateTime = DateTime.now) extends ElectionStateVotes(prev, prev.votes) with HasHistory
case class Mixing[T <: Nat](mixes: Sized[List[ShuffleResultDTO], T], prev: ElectionStateVotes) extends ElectionStateVotes(prev, prev.votes) with HasHistory
case class Mixed(prev: Mixing[_ <: Nat]) extends ElectionStateVotes(prev, prev.votes) with HasHistory
case class Decryptions[T <: Nat](decryptions: Sized[List[PartialDecryptionDTO], T], prev: ElectionStateVotes) extends ElectionStateVotes(prev, prev.votes) with HasHistory
case class Decrypted(decrypted: Seq[String], prev: Decryptions[_ <: Nat]) extends ElectionStateVotes(prev, prev.votes) with HasHistory

/**
 * The state machine transitions
 *
 * Method signatures allow the compiler to enforce the state machine logic.
 */
trait ElectionTrait {
  def create[W <: Nat : ToInt](id: String, bits: Int) : Future[Election[W, Created]]
  def startShares[W <: Nat](in: Election[W, Created]) : Future[Election[W, Shares[_0]]]
  def addShare[W <: Nat, T <: Nat](in: Election[W, Shares[T]], share: EncryptionKeyShareDTO, proverId: String)(implicit ev: T < W) : Future[Election[W, Shares[Succ[T]]]]
  def combineShares[W <: Nat](in: Election[W, Shares[W]]) : Future[Election[W, Combined]]
  def startVotes[W <: Nat](in: Election[W, Combined]) : Future[Election[W, Votes]]
  def addVote[W <: Nat](in: Election[W, Votes], vote: String) : Future[Election[W, Votes]]
  def addVotes[W <: Nat](in: Election[W, Votes], votes: List[String]) : Future[Election[W, Votes]]
  def stopVotes[W <: Nat](in: Election[W, Votes]) : Future[Election[W, VotesStopped]]
  def startMixing[W <: Nat](in: Election[W, VotesStopped]) : Future[Election[W, Mixing[_0]]]
  def addMix[W <: Nat, T <: Nat](in: Election[W, Mixing[T]], mix: ShuffleResultDTO, proverId: String)(implicit ev: T < W) : Future[Election[W, Mixing[Succ[T]]]]
  def stopMixing[W <: Nat](in: Election[W, Mixing[W]]) : Future[Election[W, Mixed]]
  def startDecryptions[W <: Nat](in: Election[W, Mixed]) : Future[Election[W, Decryptions[_0]]]
  def addDecryption[W <: Nat, T <: Nat](in: Election[W, Decryptions[T]], decryption: PartialDecryptionDTO, proverId: String)(implicit ev: T < W) : Future[Election[W, Decryptions[Succ[T]]]]
  def combineDecryptions[W <: Nat](in: Election[W, Decryptions[W]]) : Future[Election[W, Decrypted]]
}
  
object Election extends DefaultElectionImpl
{ }


/*
 * We use this to generate the entire history for an election.
 * Elections are purely functional, the result is similar to an immutable log
 */
trait HasHistory {
  def prev: ElectionState

  def printHistory(): Unit = {
    println(s"> $this")
    prev match {
      case s1: HasHistory => s1.printHistory
      case s2: ElectionState => println(s"> $s2")
    }
  }
}

/**
 * Convenience election states used to carry information in the election history forward
 */
abstract class ElectionState(val id: String, val cSettings: CryptoSettings)
abstract class ElectionStateShares(es: ElectionState, val allShares: List[(String, String)]) extends ElectionState(es.id, es.cSettings)
abstract class ElectionStatePk(ess: ElectionStateShares, val publicKey: String) extends ElectionStateShares(ess, ess.allShares)
abstract class ElectionStateVotes(espk: ElectionStatePk, val votes:List[String]) extends ElectionStatePk(espk, espk.publicKey)