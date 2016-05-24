package controllers

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import akka.stream.{ActorMaterializer, Materializer}
import akka.stream.scaladsl.Sink
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model._
import scala.concurrent.{Future, Promise}
import scala.util.{Try, Success, Failure}
import scala.concurrent.duration._
import scala.util._
import utils.Response
import akka.stream.scaladsl.Source

import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._
import play.api.libs.json._
import play.api.mvc.Results._
import play.api.libs.functional.syntax._
import models._
import app._
import services.BoardConfig


object Router extends Response
{
  implicit val system = ActorSystem()
  implicit val executor = system.dispatchers.lookup("my-blocking-dispatcher")
  implicit val materializer = ActorMaterializer()
  
  private var voteFunc : (RequestContext, Long, String) => Future[HttpResponse]  =
  {
    (ctx, electionId, voterId) => Future { HttpResponse(status = 400, entity = Json.stringify(error(s"Not Implemented", ErrorCodes.EO_ERROR)) ) }
  }
  
  val route : Route = {
    path("accumulate") {
      post {
        ctx =>
          ctx.complete {
            BoardReader.accumulate(ctx.request)
          }
      }
    } ~
    path("api" / "election" / LongNumber) { electionId =>
      pathEnd {
        get { ctx =>
          println(s"Router /api/election/$electionId")
          ctx.complete {
            BoardReader.getElectionInfo(electionId)
          }
        }
      }
    } ~
    path("api" / "election" / LongNumber / "voter" / Segment) { (electionId, voterId) =>
      pathEnd {
        post { ctx =>
          println(s"Router /api/election/$electionId/voter/$voterId")
          ctx.complete {
            voteFunc(ctx, electionId, voterId)
          }
        }
      }
    } ~
    path(Segments) { segs =>
        pathEnd {
          post { ctx =>
            println(s"Router segments " + segs.toString)
            ctx.complete {
              ""
            }
          }
        }
      }
  }
  
  private var portNumber = Promise[Int]()
  
  portNumber.future onSuccess { case port =>
    println("port is " + port)
  }
  
  
  tryBindPortRange(BoardConfig.server.startPort, route,BoardConfig.server.portRange)
    
  def tryBindPortRange(port: Int, myRoute : Route, counter: Int) {
    println("countdown counter: " + counter)
    if(counter >= 0) {
      bindPort(port, myRoute) onFailure { case err =>
        if(counter > 0) {
          Future {
            tryBindPortRange(port + 1, myRoute, counter - 1)
          }
        } else {
          if(!portNumber.isCompleted) {
            println(err, "FF     Failed to bind to {}:{}!", "localhost", port)
            portNumber.failure(err)
          }
        }
      }
    }
  }
  
  def bindPort(port: Int, myRoute : Route): Future[Http.ServerBinding] = {
    Http(system)
    .bindAndHandle(
        handler = myRoute, 
        interface = BoardConfig.server.interface,
        port = port) map 
    { 
      future =>
        portNumber.success(port)
        future
    }
  }
  
  def getPort(): Future[Int] =  {
    portNumber.future 
  }
  
  def init() = {
  }
  
  def close() {
    system.terminate()
  }
  
  def setVoteFunc(newFunc: (RequestContext, Long, String) => Future[HttpResponse]) = {
    voteFunc = newFunc
  }
}
