package app

import scala.util.{Try, Success, Failure}
//import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import scala.concurrent.{blocking, Future, Promise}
import shapeless._
import nat._
import shapeless.ops._
import ops.nat._
import controllers._
import mpservice._
import shapeless._
import syntax.sized._
import LT._

// N should be the predecessor of the number you want, I've been unable to use Pred[N] so I use Succ[N]
class ElectionAuthority[W <: Nat : ToInt , N <: Nat : ToInt ]() (implicit r : N < W){
  implicit val system = ActorSystem()
  implicit val executor = system.dispatchers.lookup("my-other-dispatcher")
  implicit val materializer = ActorMaterializer()
  BoardReader.addElectionCreationListener{ uid =>
   processElection(uid)
  }
  
  private def processElection(uid: String) = Future {
    // create the keymakers
    // these are responsible for distributed key generation and joint decryption
    val kn = new KeyMakerTrustee("keymaker " + toInt[Succ[N]] + " for election " + uid)
    // create the mixers
    // these are responsible for shuffling the votes
    val mn = new MixerTrustee("mixer " + toInt[Succ[N]] + " for election " + uid)
    
    val subscriber = BoardReader.getSubscriber(uid)
    val addShare = subscriber.addShare[N]()
    
    addShare flatMap { addShare =>
      val addShareInstance = addShare.asInstanceOf[Election[W, Shares[N]]]
      kn.createKeyShare(addShareInstance) flatMap { keyShare =>
        Election.addShare(addShareInstance, keyShare, kn.id)
      }
    }
    
      // we compose futures, first mix then second mix
    subscriber.stopVotes() map { stopVotes =>
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
    val pdNFuture = subscriber.startDecryptions() flatMap { startDecryptions =>
      Future {
        kn.partialDecryption(startDecryptions)
      }
    }
    
    // once all the mixes are finished we proceed to decryption
    subscriber.addDecryption[N]() flatMap { partialN =>
      pdNFuture flatMap { pdN =>
          Election.addDecryption(partialN.asInstanceOf[Election[W, Decryptions[N]]], pdN, kn.id)
      }
    }
  
  }
}