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

import play.api.libs.json._
import play.api.mvc.Results._
import play.api.libs.functional.syntax._
import models._
import app._

object Router
{
  implicit val system = ActorSystem()
  implicit val executor = system.dispatcher
  implicit val materializer = ActorMaterializer()
  
  private var portNumber = Promise[Int]()
  
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
  
  val requestHandler: HttpRequest => Future[HttpResponse] = {
    case HttpRequest(POST, Uri.Path("/accumulate"), _, entity, _) =>
      val promise = Promise[HttpResponse]()
      Future {
        getString(entity) onComplete {
         case Success(bodyStr) =>
           println(s"Router accumulate: $bodyStr")
           promise.completeWith(BoardReader.accumulate(bodyStr))
         case Failure(e) =>
           promise.success(HttpResponse(400, entity = s"Error $e"))
        }  
      }
      promise.future
    case _: HttpRequest =>
      Future { HttpResponse(404, entity = "Unknown resource!") }
  }
  
  bindIt(9800, requestHandler) onFailure { case err => 
    bindIt(9801, requestHandler) onFailure { case err =>
      bindIt(9802, requestHandler) onFailure { case err =>
        bindIt(9803, requestHandler) onFailure { case err =>
          portNumber.failure(err)
          println(err, "FF     Failed to bind to {}:{}!", "localhost", 9803)
        }
      }
    }
  }
  
  portNumber.future onSuccess { case port =>
    println("port is " + port)    
  }
  
  def bindIt(port: Int, requestHandler: HttpRequest => Future[HttpResponse]): Future[Http.ServerBinding] = {
    var serverSource = Http(system).bind(interface = "localhost", port = port)
    serverSource.to(Sink.foreach { connection => // foreach materializes the source
      // ... and then actually handle the connection
       connection handleWithAsyncHandler requestHandler
    }).run() map { future =>
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
    //ws.close()
    system.terminate()
  }
}
