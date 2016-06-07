package accumulator

import app._
import models._
import scala.concurrent.{Future, Promise}
import play.api.libs.json._
import play.api.libs.ws._
import scala.util.{Try, Success, Failure}
import akka.http.scaladsl.model._
import utils.BoardConfig
import utils._
import controllers._
import election._
import election.ElectionJsonFormatter

object BoardReader
  extends ElectionJsonFormatter
  with PostOffice
  with FiwareJSONFormatter
  with BoardJSONFormatter
  with HttpEntityToString
{

  var subscriptionId = ""
  private val futureSubscriptionId = getSubscriptionId(BoardPoster.getWSClient )
  
  futureSubscriptionId onComplete {
    case Success(id) =>
      subscriptionId = id
    case Failure(err) => 
      throw err
  }
  
  
  def init() {
  }
  
  private def unsubscribe(id: String, ws: WSClient) = {
    println("GG unsubscribe")
    Router.getPort() map { port =>
      val unsubsReq = UnsubscribeRequest(id, s"http://localhost:${port}/accumulate")
      println(Json.stringify(Json.toJson(unsubsReq)))
      val futureResponse: Future[WSResponse] = 
      ws.url(s"${BoardConfig.agoraboard.url}/bulletin_unsubscribe")
          .withHeaders(
            "Content-Type" -> "application/json",
            "Accept" -> "application/json")
          .post(Json.toJson(unsubsReq))
      futureResponse onComplete {
        case Success(noErr) =>
          println("Unsubscribe SUCCESS " + noErr)
        case Failure(err) =>
          println("Unsubscribe ERROR " + getMessageFromThrowable(err))
      }
    }
  }
  
  def getSubscriptionId(ws: WSClient) : Future[String] =  {
    val promise = Promise[String]
    Future {
      println("GG getSubscriptionId")
      Router.getPort() onComplete { 
        case Success(port) =>
          val acc = SubscribeRequest("election", "#", s"http://localhost:${port}/accumulate")
          val futureResponse: Future[WSResponse] = 
          ws.url(s"${BoardConfig.agoraboard.url}/bulletin_subscribe")
          .withHeaders(
            "Content-Type" -> "application/json",
            "Accept" -> "application/json")
          .post(Json.toJson(acc))
          
          futureResponse onComplete {
            case Success(response) =>
              promise.success(response.body)
              println(s"ElectionCreateSubscriber Success: ${response.body}")
            case Failure(err) =>
              println(s"ElectionCreateSubscriber Failure: ${getMessageFromThrowable(err)}")
              promise.failure(err)
          }
        case Failure(err) =>
          promise.failure(err)
      }
    }
    promise.future
  }
  
  def accumulate(request: HttpRequest) : Future[HttpResponse] = {
    val promise = Promise[HttpResponse]()
    Future {
      println(s"GG accumulate")
      getString(request.entity) map { bodyStr =>
        val json = Json.parse(bodyStr)
        json.validate[AccumulateRequest] match {
          case sr: JsSuccess[AccumulateRequest] =>
            val accRequest = sr.get
            if (accRequest.subscriptionId == subscriptionId) {
              var jsonError: Option[String] = None
              val postSeq = accRequest.contextResponses flatMap {  x => 
                x.contextElement.attributes flatMap { y =>
                  y.value.validate[Post] match {
                    case post: JsSuccess[Post] =>
                      Some(post.get)
                    case e: JsError =>
                      val str = "Accumulate has a None: this is not " +
                              s"a valid Post: ${y.value}! error: $json"
                      println(str)
                      jsonError = Some(str)
                      None
                    }
                  }
               }
               jsonError match {
                 case Some(e) =>
                   promise.success(HttpResponse(400, entity = e))
                 case None =>
                   push(postSeq)
                   promise.success(HttpResponse(200, entity = s"{}"))
               }
            } else {
              if(futureSubscriptionId.isCompleted) {
                // Error, we are receiving subscription messages from a wrong subscription id
                promise.success(HttpResponse(400))
                // Remove subscription for that subscription id
                unsubscribe(accRequest.subscriptionId, BoardPoster.getWSClient)
              } else {
                futureSubscriptionId onComplete {
                  case Success(id) =>
                    promise.completeWith(accumulate(request))
                  case Failure(err) => 
                    promise.failure(err)
                }
              }
            }
         case e: JsError =>
           val errorText = s"Bad request: invalid AccumulateRequest json: $bodyStr\nerror: ${e}\n"
             println(errorText)
             promise.success(HttpResponse(400, entity = errorText))
        }
      } recover { case err =>
        promise.trySuccess(HttpResponse(400, entity = getMessageFromThrowable(err)))
      }
    } recover { case err =>
      promise.trySuccess(HttpResponse(400, entity = getMessageFromThrowable(err)))
    }
    promise.future
  }
  
  def push(seqPost: Seq[Post]) = {
    seqPost foreach { post => 
      add(post)
    }
  } 
  
}