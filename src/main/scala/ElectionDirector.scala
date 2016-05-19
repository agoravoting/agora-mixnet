package app

import scala.util.{Try, Success, Failure}
//import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import scala.concurrent.{blocking, Future, Promise}
import shapeless._
import nat._
import ops.nat._
import controllers._
import mpservice._

class ElectionDirector[N <: Nat : ToInt](val totalVotes: Int) {  
  implicit val system = ActorSystem()
  implicit val executor = system.dispatchers.lookup("my-other-dispatcher")
  implicit val materializer = ActorMaterializer()
  BoardReader.addElectionCreationListener{ uid =>
   val subscriberCreatePromise = blocking { getOrAddCreateNotification(uid, Promise[Unit]()) }
   subscriberCreatePromise.success({})
   processElection(uid)
  }
  
  private def processElection(uid: String) = Future {
    val subscriber = BoardReader.getSubscriber(uid)
    val create = subscriber.create()
    create map { start =>
      Election.startShares(start.asInstanceOf[Election[N, Created]])
    }
    val combined = subscriber.addShare[N]() flatMap { nShares =>
      Election.combineShares(nShares.asInstanceOf[Election[N, Shares[N]]])
    }
    
    // generate dummy votes
    val plaintexts = Seq.fill(totalVotes)(scala.util.Random.nextInt(1000))
    
    val electionGettingVotes = combined flatMap { combined => 
      val startVotes = Election.startVotes(combined)
      
      // since we are storing information in election as if it were a bulletin board, all
      // the data is stored in a wire-compatible format, that is strings/jsons whatever
      // we reconstruct the public key as if it had been read from such a format
      val publicKey = Util.getPublicKeyFromString(combined.state.publicKey, combined.state.cSettings.generator)
      // encrypt the votes with the public key of the election
      val votes = Util.encryptVotes(plaintexts, combined.state.cSettings, publicKey)
      
      startVotes flatMap { startVotes => 
        // doing this in one step to avoid memory explosion
        Election.addVotes(startVotes, votes.map(_.convertToString).toList)
      }
    }
    // we are only timing the mixing phase
    var mixingStart = System.currentTimeMillis()
    var mixingEnd = mixingStart
    // stop the voting period
    val stopVotes = electionGettingVotes flatMap { electionGettingVotes => 
      // FIXME remove this
      MPBridge.total = 0;
      mixingStart = System.currentTimeMillis()
      Election.stopVotes(electionGettingVotes)
    }
    
    val startMixing = stopVotes flatMap { stopVotes =>
      Election.startMixing(stopVotes)
    }
    val lastMix = subscriber.addMix[N]()
    
    val stopMix = lastMix flatMap { lastMix =>
      Election.stopMixing(lastMix.asInstanceOf[Election[N, Mixing[N]]])
    }
    
    stopMix flatMap { stopMix =>
      Election.startDecryptions(stopMix)
    }
    
    val electionDone = subscriber.addDecryption[N]() flatMap { partialN =>
      Election.combineDecryptions(partialN.asInstanceOf[Election[N, Decryptions[N]]])
    }
    
    electionDone map { electionDone => 
      // lets check that everything went well
      // println(s"Plaintexts $plaintexts")
      // println(s"Decrypted ${electionDone.state.decrypted}")
      // println("ok: " + (plaintexts.sorted == electionDone.state.decrypted.map(_.toInt).sorted))

      val mixTime = (mixingEnd - mixingStart) / 1000.0
      val totalTime = (System.currentTimeMillis() - mixingStart) / 1000.0

      println("*************************************************************")
      println(s"finished run with votes = $totalVotes")
      println(s"mixTime: $mixTime")
      println(s"totalTime: $totalTime")
      println(s"sec / vote (mix): ${mixTime / totalVotes}")
      println(s"sec / vote: ${totalTime / totalVotes}")
      println(s"total modExps: ${MPBridge.total}")
      println(s"found modExps: ${MPBridge.found}")
      println(s"found modExps %: ${MPBridge.found/MPBridge.total.toDouble}")
      println(s"extracted modExps: ${MPBridge.getExtracted}")
      println(s"extracted modExps %: ${MPBridge.getExtracted/MPBridge.total.toDouble}")
      println(s"modExps / vote: ${MPBridge.total.toFloat / totalVotes}")
      println("*************************************************************")

      MPBridgeS.shutdown
    }
  }
  
  private var creationNotificationsMap = Map[String, Promise[Unit]]()
  
  private def getOrAddCreateNotification(key: String, promise: Promise[Unit]) : Promise[Unit] = {
    creationNotificationsMap.synchronized {
      creationNotificationsMap.get(key) match {
        case Some(value) =>
          value
        case None =>
          creationNotificationsMap += (key -> promise)
          promise
      }
    }
    
  }
  
  def newElection() : Future[String] = {
    val promise = Promise[String]()
    Future {
      // create the election,
      // we are using privacy level 2, two trustees of each kind
      // we are 2048 bits for the size of the group modulus
      val start = Election.create[N]("my election", 2048)
      start onComplete {
        case Success(election) =>
          val subscriberCreatePromise = blocking { getOrAddCreateNotification(election.state.uid, Promise[Unit]()) }
          subscriberCreatePromise.future onComplete {
            case Success(d) =>
              promise.success(election.state.uid)
            case Failure(err) =>
              promise.failure(err)
          }
        case Failure(err) =>
          promise.failure(err)
      }
    }
    promise.future
  }
  
}