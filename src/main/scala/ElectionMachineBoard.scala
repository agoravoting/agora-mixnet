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

object BoardPoster extends ElectionMachineJSONConverter
{

  implicit val system = ActorSystem("BoardPoster")
  implicit val materializer = ActorMaterializer()
  val controller = new MyController()
  implicit val ws = controller.getWS()
  
  val subscriber = new ElectionCreateSubscriber(ws)
  
  var agoraBoard = "http://172.17.0.1:9500"
  
  def setAgoraBoard(str: String) {
    agoraBoard = str
  }
  
  def create[W <: Nat: ToInt](election: Election[W, Created]) : Future[Election[W, Created]] = {    
    val futureResponse: Future[WSResponse] = 
    ws.url(s"${agoraBoard}/election/create")
    .withHeaders(
      "Content-Type" -> "application/json",
      "Accept" -> "application/json")
    .post(CreatedToJS(election))
    
    futureResponse map { success =>
      println("Success! \n" + success.body)
      // create new election with the unique id that is contained in success.body
      new Election[W, Created](Created(election.state.id, election.state.cSettings, success.body))
    }
  }
  
  def closeSystem() {
    ws.close()
    system.terminate()
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
  def startShares[W <: Nat](in: Election[W, Created]) : Future[Election[W, Shares[_0]]] = {
    BaseImpl.startShares(in) map { election =>
      //println(s"RR $election")
      election
    }
  }

  // verify and add a share
  def addShare[W <: Nat, T <: Nat](in: Election[W, Shares[T]], share: EncryptionKeyShareDTO, proverId: String)(implicit ev: T < W) : Future[Election[W, Shares[Succ[T]]]] = {
    BaseImpl.addShare(in, share, proverId) map { election => 
      //println(s"RR $election")
      election
    }
  }

  // combine the shares into a public key, can only happen if we have all the shares
  def combineShares[W <: Nat](in: Election[W, Shares[W]]) : Future[Election[W, Combined]] = {
    BaseImpl.combineShares(in) map { election =>
      //println(s"RR $election")
      election
    }
  }

  // start the voting period
  def startVotes[W <: Nat](in: Election[W, Combined]) : Future[Election[W, Votes]] = {
    BaseImpl.startVotes(in)
  }

  // votes are cast here
  def addVote[W <: Nat](in: Election[W, Votes], vote: String) : Future[Election[W, Votes]] = {
    BaseImpl.addVote(in, vote)
  }

  // votes are cast here
  def addVotes[W <: Nat](in: Election[W, Votes], votes: List[String]) : Future[Election[W, Votes]] = {
    BaseImpl.addVotes(in, votes)
  }

  // stop election period
  def stopVotes[W <: Nat](in: Election[W, Votes]) : Future[Election[W, VotesStopped]] = {
    BaseImpl.stopVotes(in)
  }

  // start mixing
  def startMixing[W <: Nat](in: Election[W, VotesStopped]) : Future[Election[W, Mixing[_0]]] = {
    BaseImpl.startMixing(in)
  }

  // add a mix by a mixer trustee
  def addMix[W <: Nat, T <: Nat](in: Election[W, Mixing[T]], mix: ShuffleResultDTO, proverId: String)(implicit ev: T < W) : Future[Election[W, Mixing[Succ[T]]]] = {
    BaseImpl.addMix(in, mix, proverId)
  }

  // stop receiving mixes, can only happen if we have all the mixes
  def stopMixing[W <: Nat](in: Election[W, Mixing[W]]) : Future[Election[W, Mixed]] = {
    BaseImpl.stopMixing(in)
  }

  // start receiving partial decryptions
  def startDecryptions[W <: Nat](in: Election[W, Mixed]) : Future[Election[W, Decryptions[_0]]] = {
    BaseImpl.startDecryptions(in)
  }

  // verify and add a partial decryption
  def addDecryption[W <: Nat, T <: Nat](in: Election[W, Decryptions[T]], decryption: PartialDecryptionDTO, proverId: String)(implicit ev: T < W) : Future[Election[W, Decryptions[Succ[T]]]] = {
    BaseImpl.addDecryption(in, decryption, proverId)
  }

  // combine partial decryptions, can only happen if we have all of them
  def combineDecryptions[W <: Nat](in: Election[W, Decryptions[W]]) : Future[Election[W, Decrypted]] = {
    BaseImpl.combineDecryptions(in) map { election =>
      BoardPoster.closeSystem()
      controllers.Router.close()
      election
    }
  }
}