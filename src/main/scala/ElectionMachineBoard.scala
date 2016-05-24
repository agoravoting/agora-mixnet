package app

import shapeless._
import nat._
import syntax.sized._
import ops.nat._
import LT._
import javax.inject._
import play.api.libs.ws._
import play.api.libs.ws.ahc._

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import scala.util.{Try, Success, Failure}
import scala.concurrent.{Future, Promise}
import com.github.nscala_time.time.Imports._
import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import play.api._
import java.io.File
import java.math.BigInteger;
import java.util.concurrent.TimeUnit._

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

import javax.inject.Inject
import scala.concurrent.duration._

import play.api.mvc._
import play.api.http.HttpEntity

import akka.actor.Actor
import akka.stream.Materializer
import akka.actor.ActorSystem
import akka.actor.Props
import akka.stream.ActorMaterializer
import akka.stream.scaladsl._
import akka.util.ByteString

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import services._
import models._
import controllers._

class MyController @Inject()
(implicit val mat: Materializer) 
{
  val classLoader = Thread.currentThread().getContextClassLoader
  val env = new play.api.Environment(new File("."), classLoader, Mode.Dev)
  val rawConfig = play.api.Configuration.load(env)
  val ws = createClient(rawConfig)
  
  def getWS() : WSClient = {
    ws
  }
  
  def createClient(rawConfig: play.api.Configuration): WSClient = {
    val parser = new WSConfigParser(rawConfig, new Environment(new File("."), classLoader, Mode.Test))
    val clientConfig = new AhcWSClientConfig(parser.parse())
    // Debug flags only take effect in JSSE when DebugConfiguration().configure is called.
    //import play.api.libs.ws.ssl.debug.DebugConfiguration
    //clientConfig.ssl.map {
    //   _.debug.map(new DebugConfiguration().configure)
    //}
    val builder = new AhcConfigBuilder(clientConfig)
    val client = new AhcWSClient(builder.build())
    client
  }
}

object BoardPoster extends ElectionMachineJSONConverter with BoardJSONFormatter
{
  implicit private val system = ActorSystem("BoardPoster")
  implicit private val materializer = ActorMaterializer()
  implicit val executor = system.dispatchers.lookup("my-other-dispatcher")
  private val controller = new MyController()
  implicit private val ws = controller.getWS()
  
  def getWSClient() : WSClient = ws
      
  def create[W <: Nat: ToInt](election: Election[W, Created]) : Future[Election[W, Created]] = {
    val promise = Promise[Election[W, Created]]()
    Future {
      val futureResponse: Future[WSResponse] = 
      ws.url(s"${BoardConfig.agoraboard.url}/bulletin_post")
      .withHeaders(
        "Content-Type" -> "application/json",
        "Accept" -> "application/json")
      .post(Json.toJson(CreatedToPostRequest(election)))
      
      futureResponse onFailure { case err =>
        promise.failure(err)
      }
      
      futureResponse onSuccess { case response =>
       response.json.validate[BoardAttributes] match { 
         case attr: JsSuccess[BoardAttributes] =>
           println("Success! \n" + response.json)
           // The post message index will be the unique id of the election
           promise.success(new Election[W, Created](Created(election.state.id, election.state.cSettings, attr.get.index)))
         case JsError(e) =>
           promise.failure(new Error(s"$e"))
       }
      }
    }
    promise.future
  }
  
  def closeSystem() {
    println("terminating....!")
    ws.close()
    system.terminate()
    controllers.Router.close()
  }
  
  def addShare[W <: Nat : ToInt, T <: Nat : ToInt](election: Election[W, Shares[T]]) : Future[Election[W, Shares[T]]] =  {
    val promise = Promise[Election[W, Shares[T]]]()
    Future {
      val futureResponse: Future[WSResponse] = 
      ws.url(s"${BoardConfig.agoraboard.url}/bulletin_post")
      .withHeaders(
        "Content-Type" -> "application/json",
        "Accept" -> "application/json")
      .post(Json.toJson(SharesToPostRequest(election)))
     
      futureResponse onFailure { case err =>
        promise.failure(err)
      }
      
      futureResponse onSuccess { case response =>
       promise.success(election)
      }
    }
    promise.future
  }
  
  def combineShares[W <: Nat : ToInt](election: Election[W, Combined]) : Future[Election[W, Combined]] = {
    val promise = Promise[Election[W, Combined]]()
    Future {
      val futureResponse: Future[WSResponse] = 
      ws.url(s"${BoardConfig.agoraboard.url}/bulletin_post")
      .withHeaders(
        "Content-Type" -> "application/json",
        "Accept" -> "application/json")
      .post(Json.toJson(CombinedToPostRequest(election)))
     
      futureResponse onFailure { case err =>
        promise.failure(err)
      }
      
      futureResponse onSuccess { case response =>
       promise.success(election)
      }
    }
    promise.future
  }
  
  def addVotes[W <: Nat : ToInt](election: Election[W, Votes], votes: List[String]) : Future[Election[W, Votes]] = {
    val promise = Promise[Election[W, Votes]]()
    Future {
      if(0 > election.state.addVoteIndex) {
        promise.failure(new Error(s"Error: addVoteIndex < 0 : ${election.state.addVoteIndex}"))
      } else if(0 == election.state.addVoteIndex && 0 < votes.length) {
        promise.failure(new Error("Error: addVoteIndex is 0 but it has nonzero votes"))
      } else if (election.state.addVoteIndex > 0 && 0 == votes.length ) {
        promise.failure(new Error("Error: addVoteIndex is not zero but it has zero votes"))
      } else {
        val futureResponse: Future[WSResponse] = 
        ws.url(s"${BoardConfig.agoraboard.url}/bulletin_post")
        .withHeaders(
          "Content-Type" -> "application/json",
          "Accept" -> "application/json")
        .post(Json.toJson(VotesToPostRequest(election, votes)))
       
        futureResponse onFailure { case err =>
          promise.failure(err)
        }
        
        futureResponse onSuccess { case response =>
         promise.success(election)
        }
      }
    }
    promise.future
  }
  
  def stopVotes[W <: Nat : ToInt](election: Election[W, VotesStopped]) : Future[Election[W, VotesStopped]] = {
    val promise = Promise[Election[W, VotesStopped]]()
    Future {
      val futureResponse: Future[WSResponse] = 
      ws.url(s"${BoardConfig.agoraboard.url}/bulletin_post")
      .withHeaders(
        "Content-Type" -> "application/json",
        "Accept" -> "application/json")
      .post(Json.toJson(VotesStoppedToPostRequest(election)))
     
      futureResponse onFailure { case err =>
        promise.failure(err)
      }
      
      futureResponse onSuccess { case response =>
       promise.success(election)
      }
    }
    promise.future
  }
  
  def startMixing[W <: Nat : ToInt](election: Election[W, Mixing[_0]]) : Future[Election[W, Mixing[_0]]] = {
    val promise = Promise[Election[W, Mixing[_0]]]()
    Future {
      val futureResponse: Future[WSResponse] = 
      ws.url(s"${BoardConfig.agoraboard.url}/bulletin_post")
      .withHeaders(
        "Content-Type" -> "application/json",
        "Accept" -> "application/json")
      .post(Json.toJson(StartMixingToPostRequest(election)))
     
      futureResponse onFailure { case err =>
        promise.failure(err)
      }
      
      futureResponse onSuccess { case response =>
       promise.success(election)
      }
    }
    promise.future
  }
  
  def addMix[W <: Nat : ToInt, T <: Nat : ToInt](election: Election[W, Mixing[T]], mix: ShuffleResultDTO) : Future[Election[W, Mixing[T]]] = {
    val promise = Promise[Election[W, Mixing[T]]]()
    Future {
      val postReq = MixingToPostRequest(election, mix)
      println("GG MixingToPostRequest 1")
      val jsPostReq = Json.toJson(postReq)
      println("GG MixingToPostRequest 2")
      val futureResponse: Future[WSResponse] = 
      ws.url(s"${BoardConfig.agoraboard.url}/bulletin_post")
      .withHeaders(
        "Content-Type" -> "application/json",
        "Accept" -> "application/json")
      .post(jsPostReq)
     
      futureResponse onFailure { case err =>
        promise.failure(err)
      }
      
      futureResponse onSuccess { case response =>
       println("GG MixingToPostRequest 3")
       promise.success(election)
      }
    }
    promise.future
  }
  
  
  def stopMixing[W <: Nat : ToInt](election: Election[W, Mixed]) : Future[Election[W, Mixed]] = {
    val promise = Promise[Election[W, Mixed]]()
    Future {
      val futureResponse: Future[WSResponse] = 
      ws.url(s"${BoardConfig.agoraboard.url}/bulletin_post")
      .withHeaders(
        "Content-Type" -> "application/json",
        "Accept" -> "application/json")
      .post(Json.toJson(MixedToPostRequest(election)))
     
      futureResponse onFailure { case err =>
        promise.failure(err)
      }
      
      futureResponse onSuccess { case response =>
       promise.success(election)
      }
    }
    promise.future
  }
  
  
  def startDecryptions[W <: Nat : ToInt](election: Election[W, Decryptions[_0]]) : Future[Election[W, Decryptions[_0]]] = {
    val promise = Promise[Election[W, Decryptions[_0]]]()
    Future {
      val futureResponse: Future[WSResponse] = 
      ws.url(s"${BoardConfig.agoraboard.url}/bulletin_post")
      .withHeaders(
        "Content-Type" -> "application/json",
        "Accept" -> "application/json")
      .post(Json.toJson(StartDecryptionsToPostRequest(election)))
     
      futureResponse onFailure { case err =>
        promise.failure(err)
      }
      
      futureResponse onSuccess { case response =>
       promise.success(election)
      }
    }
    promise.future
  }

  def combineDecryptions[W <: Nat : ToInt](election: Election[W, Decrypted]) : Future[Election[W, Decrypted]] = {
    val promise = Promise[Election[W, Decrypted]]()
    Future {
      val futureResponse: Future[WSResponse] = 
      ws.url(s"${BoardConfig.agoraboard.url}/bulletin_post")
      .withHeaders(
        "Content-Type" -> "application/json",
        "Accept" -> "application/json")
      .post(Json.toJson(DecryptedToPostRequest(election)))
     
      futureResponse onFailure { case err =>
        promise.failure(err)
      }
      
      futureResponse onSuccess { case response =>
       promise.success(election)
      }
    }
    promise.future
  }
  
  

  def addDecryption[W <: Nat : ToInt, T <: Nat : ToInt](election: Election[W, Decryptions[T]], decryption: PartialDecryptionDTO) : Future[Election[W, Decryptions[T]]] = {
    val promise = Promise[Election[W, Decryptions[T]]]()
    Future {
      val futureResponse: Future[WSResponse] = 
      ws.url(s"${BoardConfig.agoraboard.url}/bulletin_post")
      .withHeaders(
        "Content-Type" -> "application/json",
        "Accept" -> "application/json")
      .post(Json.toJson(AddDecryptionToPostRequest(election, decryption)))
     
      futureResponse onFailure { case err =>
        promise.failure(err)
      }
      
      futureResponse onSuccess { case response =>
       promise.success(election)
      }
    }
    promise.future
  }
}

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
          println("== startVotes promise " + election.state.uid)
          promise.success(election)
          BoardPoster.addVotes(election, List[String]()) map { e =>
            println("== startVotes posted " + election.state.uid)
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