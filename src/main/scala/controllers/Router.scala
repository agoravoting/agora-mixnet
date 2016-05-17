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
import akka.stream.scaladsl.Source

import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._
import play.api.libs.json._
import play.api.mvc.Results._
import play.api.libs.functional.syntax._
import models._
import app._

object Router
{
  implicit val system = ActorSystem()
  implicit val executor = system.dispatchers.lookup("my-blocking-dispatcher")//system.dispatcher
  implicit val materializer = ActorMaterializer()
  
  
  val route : Route = {
    path("accumulate") {
      post {
        ctx =>
          ctx.complete {
            val promise = Promise[HttpResponse]()
            Future {
              getString(ctx.request.entity) onComplete {
                case Success(bodyStr) =>
                  println(s"Router accumulate: $bodyStr")
                  promise.completeWith(BoardReader.accumulate(bodyStr))
                case Failure(e) =>
                  promise.success(HttpResponse(400, entity = s"Error $e"))
              }  
            }
            promise.future
          }
      }
    } ~
    path("api" / "election" / LongNumber) { electionId =>
      pathEnd {
        get { ctx =>
          ctx.complete {
            println(s"Router /api/election/$electionId")
            BoardReader.getElectionInfo(electionId)
          }
        }
      }
    }
  }
  
  private var portNumber = Promise[Int]()
  
  portNumber.future onSuccess { case port =>
    println("port is " + port)
  }
  
  def getString(entity: HttpEntity) : Future[String] =  {
    val promise = Promise[String]()
    Future {
      val future = entity.toStrict(5.seconds) map { st =>
        st.data.decodeString("UTF-8")
      }
      promise.completeWith(future)
    }
    promise.future
  }
  
  tryBindPortRange(9800, route,100)
    
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
        interface = "localhost", 
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
}
