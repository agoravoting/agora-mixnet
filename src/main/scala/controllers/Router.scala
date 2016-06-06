package controllers

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.{ActorMaterializer, Materializer}
import scala.concurrent.{Future, Promise}
import utils.Response

import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._
import utils.BoardConfig
import accumulator._
import director._

object Router extends Response
{
  implicit val system = ActorSystem()
  implicit val executor = system.dispatchers.lookup("my-blocking-dispatcher")
  implicit val materializer = ActorMaterializer()
  
  private var directorImpl : AbstractElectionDirector = new EmptyElectionDirector()
  
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
    path("api" / "election" / LongNumber) { electionId =>
      pathEnd {
        post { ctx =>
          println(s"Router create post /api/election/$electionId")
          ctx.complete {
            directorImpl.createElection(ctx, electionId)
          }
        }
      }
    } ~
    path("api" / "election" / LongNumber / "voter" / Segment) { (electionId, voterId) =>
      pathEnd {
        post { ctx =>
          println(s"Router /api/election/$electionId/voter/$voterId")
          ctx.complete {
            directorImpl.addVote(ctx, electionId, voterId)
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
  
  def setElectionDirector(director: AbstractElectionDirector) {
    directorImpl = director
  }
}
