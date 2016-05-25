package election

import shapeless._
import ops.nat._
import javax.inject._
import play.api.libs.ws._
import play.api.libs.ws.ahc._
import play.api.libs.json._
import scala.concurrent.{Future, Promise}
import play.api._
import java.io.File
import akka.stream.Materializer
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import utils.BoardConfig
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
           println("Election creation posted to immutable log. Election id is " + attr.get.index)
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