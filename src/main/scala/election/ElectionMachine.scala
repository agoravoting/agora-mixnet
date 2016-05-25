package election

import shapeless._
import ops.nat._
import LT._
import play.api.libs.functional.syntax._
import scala.util.{Try, Success, Failure}
import scala.concurrent.{Future, Promise}
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import models._
import controllers._
import election._

object BaseImpl extends DefaultElectionImpl {}
/**
 * The state machine transitions
 *
 * Method signatures allow the compiler to enforce the state machine logic.
 */
trait ElectionMachine extends ElectionTrait
{
  implicit val system = ActorSystem()
  implicit val executor = system.dispatchers.lookup("my-other-dispatcher")
  implicit val materializer = ActorMaterializer()
  // create an election
  def create[W <: Nat : ToInt](id: String, bits: Int) : Future[Election[W, Created]] = {
    val promise = Promise[Election[W, Created]]()
    Future {
      BaseImpl.create(id, bits) onComplete {
        case Success(election) =>
          // the immutable log sets the election id, so we really need to write in the log before fulfilling the promise
          promise.completeWith(BoardPoster.create(election))          
        case Failure(err) =>
          promise.failure(err)
      }
    }
    promise.future
  }

  // now ready to receive shares
  def startShares[W <: Nat : ToInt](in: Election[W, Created]) : Future[Election[W, Shares[_0]]] = {
    val promise = Promise [Election[W, Shares[_0]]]()
    Future {
      BaseImpl.startShares(in) onComplete { 
        case Success(election) =>
          promise.success(election)
          BoardPoster.addShare(election)
        case Failure(err) =>
          promise.failure(err)
      }
    }
    promise.future
  }

  // verify and add a share
  def addShare[W <: Nat : ToInt, T <: Nat : ToInt](in: Election[W, Shares[T]], share: EncryptionKeyShareDTO, proverId: String)(implicit ev: T < W) : Future[Election[W, Shares[Succ[T]]]] = {
    val index = ToInt[T].apply() +1
    val promise = Promise[Election[W, Shares[Succ[T]]]]
    Future {
      BaseImpl.addShare(in, share, proverId) onComplete {
        case Success(election) =>
          promise.success(election)
          BoardPoster.addShare(election)
        case Failure(err) =>
          promise.failure(err)
      }
    }
    promise.future
  }

  // combine the shares into a public key, can only happen if we have all the shares
  def combineShares[W <: Nat : ToInt](in: Election[W, Shares[W]]) : Future[Election[W, Combined]] = {
    val promise = Promise[Election[W, Combined]]()
    Future {
      BaseImpl.combineShares(in) onComplete { 
        case Success(election) =>
          promise.success(election)
          BoardPoster.combineShares(election)
        case Failure(err) =>
          promise.failure(err)
      }
    }
    promise.future
  }

  // start the voting period
  def startVotes[W <: Nat : ToInt](in: Election[W, Combined]) : Future[Election[W, Votes]] = {
    val promise = Promise[Election[W, Votes]]()
    Future {
      BaseImpl.startVotes(in) onComplete {
        case Success(election) =>
          promise.success(election)
          BoardPoster.addVotes(election, List[String]()) map { e =>
            println("ElectionMachineBoard::startVotes posted " + election.state.uid)
          }
        case Failure(err) =>
          promise.failure(err)
      }
    }
    promise.future
  }

  // votes are cast here
  def addVote[W <: Nat : ToInt](in: Election[W, Votes], vote: String) : Future[Election[W, Votes]] = {
    val promise = Promise[Election[W, Votes]]()
    Future {
      BaseImpl.addVote(in, vote) onComplete {
        case Success(election) =>
          promise.success(election)
          BoardPoster.addVotes(election, List[String](vote))
        case Failure(err) =>
          promise.failure(err)
      }
    }
    promise.future
  }

  // votes are cast here
  def addVotes[W <: Nat : ToInt](in: Election[W, Votes], votes: List[String]) : Future[Election[W, Votes]] = {
    val promise = Promise[Election[W, Votes]]()
    Future {
      BaseImpl.addVotes(in, votes) onComplete {
        case Success(election) =>
          promise.success(election)
          BoardPoster.addVotes(election, votes)
        case Failure(err) =>
          promise.failure(err)
      }
    }
    promise.future
  }

  // stop election period
  def stopVotes[W <: Nat : ToInt](in: Election[W, Votes]) : Future[Election[W, VotesStopped]] = {
    val promise = Promise[Election[W, VotesStopped]]()
    Future {
      BaseImpl.stopVotes(in) onComplete {
        case Success(election) =>
          promise.success(election)
          BoardPoster.stopVotes(election)
        case Failure(err) =>
          promise.failure(err)
      }
    }
    promise.future
  }

  // start mixing
  def startMixing[W <: Nat : ToInt](in: Election[W, VotesStopped]) : Future[Election[W, Mixing[_0]]] = {
    val promise = Promise[Election[W, Mixing[_0]]]()
    Future {
      BaseImpl.startMixing(in) onComplete {
        case Success(election) =>
          promise.success(election)
          BoardPoster.startMixing(election)
        case Failure(err) =>
          promise.failure(err)
      }
    }
    promise.future
  }

  // add a mix by a mixer trustee
  def addMix[W <: Nat : ToInt, T <: Nat : ToInt](in: Election[W, Mixing[T]], mix: ShuffleResultDTO, proverId: String)(implicit ev: T < W) : Future[Election[W, Mixing[Succ[T]]]] = {
    val promise = Promise[Election[W, Mixing[Succ[T]]]]()
    Future {
      BaseImpl.addMix(in, mix, proverId) onComplete {
        case Success(election) =>
          promise.success(election)
          BoardPoster.addMix(election, mix)
        case Failure(err) =>
          promise.failure(err)
      }
    }
    promise.future
  }

  // stop receiving mixes, can only happen if we have all the mixes
  def stopMixing[W <: Nat : ToInt](in: Election[W, Mixing[W]]) : Future[Election[W, Mixed]] = {
    val promise = Promise[Election[W, Mixed]]()
    Future {
      BaseImpl.stopMixing(in) onComplete {
        case Success(election) =>
          promise.success(election)
          BoardPoster.stopMixing(election)
        case Failure(err) =>
          promise.failure(err)
      }
    }
    promise.future
  }

  // start receiving partial decryptions
  def startDecryptions[W <: Nat : ToInt](in: Election[W, Mixed]) : Future[Election[W, Decryptions[_0]]] = {
    val promise = Promise[Election[W, Decryptions[_0]]]()
    Future {
      BaseImpl.startDecryptions(in) onComplete {
        case Success(election) =>
          promise.success(election)
          BoardPoster.startDecryptions(election)
        case Failure(err) =>
          promise.failure(err)
      }
    }
    promise.future
  }

  // verify and add a partial decryption
  def addDecryption[W <: Nat : ToInt, T <: Nat : ToInt](in: Election[W, Decryptions[T]], decryption: PartialDecryptionDTO, proverId: String)(implicit ev: T < W) : Future[Election[W, Decryptions[Succ[T]]]] = {
    val promise = Promise[Election[W, Decryptions[Succ[T]]]]()
    Future {
      BaseImpl.addDecryption(in, decryption, proverId) onComplete {
        case Success(election) =>
          promise.success(election)
          BoardPoster.addDecryption(election, decryption)
        case Failure(err) =>
          promise.failure(err)
      }
    }
    promise.future
  }

  // combine partial decryptions, can only happen if we have all of them
  def combineDecryptions[W <: Nat : ToInt](in: Election[W, Decryptions[W]]) : Future[Election[W, Decrypted]] = {
    val promise = Promise[Election[W, Decrypted]]()
    Future {
      BaseImpl.combineDecryptions(in) onComplete {
        case Success(election) =>
          promise.success(election)
          BoardPoster.combineDecryptions(election)
        case Failure(err) =>
          promise.failure(err)
      }
    }
    promise.future
  }
}