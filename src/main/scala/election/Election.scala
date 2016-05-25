package election

import shapeless._
import shapeless.ops.nat._
import com.github.nscala_time.time.Imports._
import accumulator.BoardReader
import models._

/**
 * An election is a typed, purely function state machine with an immutable history
 *
 * The parameters are privacy level, W, and current state, S
 *
 */
class Election[+W <: Nat : ToInt, +S <: ElectionState] (val state: S) {
  override def toString() = s"election ${state.id}, ${state.toString}"
}

/**
 * These types represent the state of the election and associated information
 *
 */
case class Created(override val id: String, override val cSettings: CryptoSettings, override val uid: String) extends ElectionState(id, cSettings, uid)
case class Shares[T <: Nat : ToInt](val shares: Sized[List[(String, String)], T], prev: ElectionState) extends ElectionStateShares(prev, shares.toList) with HasHistory
case class Combined(override val publicKey: String, prev: ElectionStateShares) extends ElectionStatePk(prev, publicKey) with HasHistory
case class Votes(votes: List[String], addVoteIndex: Int, prev: ElectionStatePk) extends ElectionStatePk(prev, prev.publicKey) with HasHistory
case class VotesStopped(lastAddVoteIndex: Int, prev: Votes, date: DateTime = DateTime.now) extends ElectionStateVotes(prev, prev.votes) with HasHistory
case class Mixing[T <: Nat : ToInt](mixes: Sized[List[ShuffleResultDTO], T], prev: ElectionStateVotes) extends ElectionStateVotes(prev, prev.votes) with HasHistory
case class Mixed(prev: Mixing[_ <: Nat]) extends ElectionStateVotes(prev, prev.votes) with HasHistory
case class Decryptions[T <: Nat : ToInt](decryptions: Sized[List[PartialDecryptionDTO], T], prev: ElectionStateVotes) extends ElectionStateVotes(prev, prev.votes) with HasHistory
case class Decrypted(decrypted: Seq[String], prev: Decryptions[_ <: Nat]) extends ElectionStateVotes(prev, prev.votes) with HasHistory

object Election extends ElectionMachine
{
  def init() = {
    BoardReader.init()
  }
}


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
abstract class ElectionState(val id: String, val cSettings: CryptoSettings,val uid: String)
abstract class ElectionStateShares(es: ElectionState, val allShares: List[(String, String)]) extends ElectionState(es.id, es.cSettings, es.uid)
abstract class ElectionStatePk(ess: ElectionStateShares, val publicKey: String) extends ElectionStateShares(ess, ess.allShares)
abstract class ElectionStateVotes(espk: ElectionStatePk, val votes:List[String]) extends ElectionStatePk(espk, espk.publicKey)