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
import scala.concurrent.ExecutionContext.Implicits.global
import services._
import models._

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
  implicit val system = ActorSystem("BoardPoster")
  implicit val materializer = ActorMaterializer()
  val controller = new MyController()
  implicit val ws = controller.getWS()
  
  val subscriber = new ElectionCreateSubscriber(ws)
    
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
    ws.close()
    system.terminate()
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
}

object BaseImpl extends DefaultElectionImpl {}
/**
 * The state machine transitions
 *
 * Method signatures allow the compiler to enforce the state machine logic.
 */
trait ElectionMachine extends ElectionTrait
{
  // create an election
  def create[W <: Nat : ToInt](id: String, bits: Int) : Future[Election[W, Created]] = { 
    controllers.Router.open()
    BaseImpl.create(id, bits) flatMap { election =>
      BoardPoster.create(election)
    }
  }

  // now ready to receive shares
  def startShares[W <: Nat : ToInt](in: Election[W, Created]) : Future[Election[W, Shares[_0]]] = {
    BaseImpl.startShares(in) flatMap { election =>
      BoardPoster.addShare(election)
    }
  }

  // verify and add a share
  def addShare[W <: Nat : ToInt, T <: Nat : ToInt](in: Election[W, Shares[T]], share: EncryptionKeyShareDTO, proverId: String)(implicit ev: T < W) : Future[Election[W, Shares[Succ[T]]]] = {
    BaseImpl.addShare(in, share, proverId) flatMap { election => 
      BoardPoster.addShare(election)
    }
  }

  // combine the shares into a public key, can only happen if we have all the shares
  def combineShares[W <: Nat : ToInt](in: Election[W, Shares[W]]) : Future[Election[W, Combined]] = {
    BaseImpl.combineShares(in) flatMap { election =>
      BoardPoster.combineShares(election)
    }
  }

  // start the voting period
  def startVotes[W <: Nat : ToInt](in: Election[W, Combined]) : Future[Election[W, Votes]] = {
    BaseImpl.startVotes(in) flatMap { election =>
      BoardPoster.addVotes(election, List[String]())
    }
  }

  // votes are cast here
  def addVote[W <: Nat : ToInt](in: Election[W, Votes], vote: String) : Future[Election[W, Votes]] = {
    BaseImpl.addVote(in, vote) flatMap { election =>
      BoardPoster.addVotes(election, List[String](vote))
    }
  }

  // votes are cast here
  def addVotes[W <: Nat : ToInt](in: Election[W, Votes], votes: List[String]) : Future[Election[W, Votes]] = {
    BaseImpl.addVotes(in, votes) flatMap { election =>
      BoardPoster.addVotes(election, votes)
    }
  }

  // stop election period
  def stopVotes[W <: Nat : ToInt](in: Election[W, Votes]) : Future[Election[W, VotesStopped]] = {
    BaseImpl.stopVotes(in) flatMap { election =>
      BoardPoster.stopVotes(election)
    }
  }

  // start mixing
  def startMixing[W <: Nat : ToInt](in: Election[W, VotesStopped]) : Future[Election[W, Mixing[_0]]] = {
    BaseImpl.startMixing(in)
  }

  // add a mix by a mixer trustee
  def addMix[W <: Nat : ToInt, T <: Nat : ToInt](in: Election[W, Mixing[T]], mix: ShuffleResultDTO, proverId: String)(implicit ev: T < W) : Future[Election[W, Mixing[Succ[T]]]] = {
    BaseImpl.addMix(in, mix, proverId)
  }

  // stop receiving mixes, can only happen if we have all the mixes
  def stopMixing[W <: Nat : ToInt](in: Election[W, Mixing[W]]) : Future[Election[W, Mixed]] = {
    BaseImpl.stopMixing(in)
  }

  // start receiving partial decryptions
  def startDecryptions[W <: Nat : ToInt](in: Election[W, Mixed]) : Future[Election[W, Decryptions[_0]]] = {
    BaseImpl.startDecryptions(in)
  }

  // verify and add a partial decryption
  def addDecryption[W <: Nat : ToInt, T <: Nat : ToInt](in: Election[W, Decryptions[T]], decryption: PartialDecryptionDTO, proverId: String)(implicit ev: T < W) : Future[Election[W, Decryptions[Succ[T]]]] = {
    BaseImpl.addDecryption(in, decryption, proverId)
  }

  // combine partial decryptions, can only happen if we have all of them
  def combineDecryptions[W <: Nat : ToInt](in: Election[W, Decryptions[W]]) : Future[Election[W, Decrypted]] = {
    BaseImpl.combineDecryptions(in) map { election =>
      BoardPoster.closeSystem()
      controllers.Router.close()
      election
    }
  }
}