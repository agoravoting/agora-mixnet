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
import scala.concurrent.duration._
import scala.util._

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


  val serverSource = Http(system).bind(interface = "localhost", port = 9800)
  
  def getString(entity: HttpEntity) : Future[String] = {
    entity.toStrict(5.seconds) map { st =>
      st.data.decodeString("UTF-8")
    }
  }
  
  val requestHandler: HttpRequest => Future[HttpResponse] = {
    case HttpRequest(POST, Uri.Path("/accumulate"), _, entity, _) =>
      val promise = Promise[HttpResponse]()
      getString(entity) onComplete {
       case Success(bodyStr) =>
         println(s"Router accumulate: $bodyStr")
         promise.completeWith(BoardReader.accumulate(bodyStr))         
       case Failure(e) =>
         promise.success(HttpResponse(400, entity = s"Error $e"))
      }  
      promise.future
    case _: HttpRequest =>
      Future { HttpResponse(404, entity = "Unknown resource!") }
  }
  
  val bindingFuture: Future[Http.ServerBinding] =
  serverSource.to(Sink.foreach { connection => // foreach materializes the source
    // ... and then actually handle the connection
     connection handleWithAsyncHandler requestHandler
  }).run()
  
  bindingFuture.onFailure {
    case ex: Exception =>
      println(ex, "FF     Failed to bind to {}:{}!", "localhost", 9800)
  }
  
  def open() = {
  }
  
  def close() {
    //ws.close()
    system.terminate()
  }
}
