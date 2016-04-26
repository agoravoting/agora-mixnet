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
import java.util.Base64
import java.nio.charset.StandardCharsets
import app._

object Router extends BoardJSONFormatter with ElectionJsonFormatter
{
  implicit val system = ActorSystem()
  implicit val executor = system.dispatcher
  implicit val materializer = ActorMaterializer()


  val serverSource = Http(system).bind(interface = "localhost", port = 9800)
  
  def getString(entity: HttpEntity) : Future[String] = {
    entity.toStrict(5.seconds) map { st =>
      //new String(st.data.decodeString("UTF-8").getBytes())
      st.data.decodeString("UTF-8")
    }
  }
  
  val requestHandler: HttpRequest => Future[HttpResponse] = {
    case HttpRequest(POST, Uri.Path("/accumulate"), _, entity, _) =>
      val promise = Promise[HttpResponse]()
      getString(entity) onComplete {
       case Success(bodyStr) =>
         println("FF ACCUMULATE")
         println(s"FF     Received: ${bodyStr}")
         val js = Json.parse(bodyStr)
         println(s"FF     JSON: ${Json.stringify(js)}")
         /*js.validate[Seq[JsValue]] match {
           case jSeqPost: JsSuccess[Seq[JsValue]] =>
             val seqJsSeq = jSeqPost.get
             seqJsSeq foreach { x =>
               println(s"FF     JSON2:"+ Json.stringify(x))
             }
             
           case e: JsError => 
             println(s"Router JsError e: $e")
         }*/
         js.validate[Seq[Post]] match {
           case jSeqPost: JsSuccess[Seq[Post]] =>
             val seqPost = jSeqPost.get
             seqPost foreach { post => 
               if(post.user_attributes.section == "election" &&
                  post.user_attributes.group == "create") {
                 val messageB64 = post.message.replace('.', '=')
                 val message = new String(Base64.getDecoder.decode(messageB64), StandardCharsets.UTF_8)
                 val jsMsg = Json.parse(message)
                 jsMsg.validate[JsElection[JsElectionState]] match {
                   case jSeqPost: JsSuccess[JsElection[JsElectionState]] =>
                     println(s"\nRouter 1 JsSuccess : ${Json.stringify(jsMsg)}")
                   case e: JsError => 
                     println(s"\nRouter 1 JsError error: ${e} message ${message}")
                 }
               } else {
                     println("\nRouter else")
               }
             }
           case e: JsError => 
             println(s"Router JsError e: $e")
         }
         promise.success(HttpResponse(entity = s"FF     Received: ${bodyStr}")) 
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
