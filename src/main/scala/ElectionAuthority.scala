package app

import scala.util.{Try, Success, Failure}
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import scala.concurrent.{blocking, Future, Promise}
import shapeless._
import nat._
import ops.nat._
import LT._
import accumulator.BoardReader
import election.Shares
import election.Mixing
import election.Election
import election.Decryptions

// N should be the predecessor of the number you want, I've been unable to use Pred[N] so I use Succ[N]
class ElectionAuthority[W <: Nat : ToInt , N <: Nat : ToInt ]() (implicit r : N < W){
  implicit val system = ActorSystem()
  implicit val executor = system.dispatchers.lookup("my-other-dispatcher")
  implicit val materializer = ActorMaterializer()
  
  
  // create the keymakers
  // these are responsible for distributed key generation and joint decryption
  val kn = new KeyMakerTrustee("keymaker " + toInt[Succ[N]])
  // create the mixers
  // these are responsible for shuffling the votes
  val mn = new MixerTrustee("mixer " + toInt[Succ[N]])

  BoardReader.addElectionCreationListener{ uid =>
   processElection(uid)
  }
  
  private def processElection(uid: String) = {
    val promise = Promise[Unit]()
    Future {
      
      val subscriber = BoardReader.getSubscriber(uid)
      val addShare = subscriber.addShare[N]()
      
      val shareAdded = addShare flatMap { addShare =>
        val addShareInstance = addShare.asInstanceOf[Election[W, Shares[N]]]
        kn.createKeyShare(addShareInstance) flatMap { keyShare =>
          Election.addShare(addShareInstance, keyShare, kn.id)
        }
      }
      
        // we compose futures, first mix then second mix
      val mixAdded = shareAdded flatMap { r => subscriber.stopVotes() } map { stopVotes =>
        val (predatan, proofn) = mn.preShuffleVotes(stopVotes)
        subscriber.addMix[N]() map { previousMix =>
          val previousMixInstance = previousMix.asInstanceOf[Election[W, Mixing[N]]]
          val shufflen = mn.shuffleVotes(previousMixInstance, predatan, proofn)
          shufflen flatMap { shuffle =>
            // the proof is verified and the shuffle is then added to the election, advancing its state
            Election.addMix(previousMixInstance, shuffle, mn.id)
          }
        }
      }
      
      val partialDecryption = mixAdded flatMap { r => subscriber.startDecryptions() } flatMap { startDecryptions =>
        Future {
          kn.partialDecryption(startDecryptions)
        }
      }
      
      // once all the mixes are finished we proceed to decryption
      partialDecryption flatMap { pdN => 
        subscriber.addDecryption[N]()  flatMap { partialN =>
          Election.addDecryption(partialN.asInstanceOf[Election[W, Decryptions[N]]], pdN, kn.id)
        }
      } map { r => 
        promise.success({})
      } recover { case err =>
        promise.tryFailure(err)
      }
    } recover { case err =>
      promise.tryFailure(err)
    }
    promise.future
  }
}