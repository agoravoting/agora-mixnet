package app

import javax.inject._
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Success, Failure}
import play.api._
import play.api.mvc._ 
import play.api.libs.json._
import play.api.libs.ws._
import play.api.Logger
import models._
import services._

 
class ElectionCreateSubscriber @Inject
(ws: WSClient)
(implicit exec: ExecutionContext)
extends BoardJSONFormatter
{
  println("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF ElectionCreateSubscriber")
  subscribeToCreate()
  def subscribeToCreate() {
    val acc = new SubscribeRequest("election", "create", "http://localhost:9800/accumulate")
    val futureResponse: Future[WSResponse] = 
    ws.url(s"${BoardConfig.agoraboard.url}/bulletin_subscribe")
    .withHeaders(
      "Content-Type" -> "application/json",
      "Accept" -> "application/json")
    .post(Json.toJson(acc))
    
    futureResponse onComplete {
      case Success(response) =>
        println(s"ElectionCreateSubscriber Success: ${response.body}")
      case Failure(e) =>
        println(s"ElectionCreateSubscriber Failure: $e")
    }
  }
}

