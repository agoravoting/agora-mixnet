package director

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import akka.http.scaladsl.server.RequestContext
import scala.concurrent.Future
import akka.http.scaladsl.model._
import play.api.libs.json._
import utils.Response

class EmptyElectionDirector extends AbstractElectionDirector with Response {
  implicit val system = ActorSystem()
  implicit val executor = system.dispatchers.lookup("my-blocking-dispatcher")
  implicit val materializer = ActorMaterializer()
  
  private def notImplemented() : Future[HttpResponse] =
  {
     Future { HttpResponse(status = 400, entity = Json.stringify(error(s"Not Implemented", ErrorCodes.EO_ERROR)) ) }
  }
  
  def addVote(ctx: RequestContext, electionId : Long, voterId: String) : Future[HttpResponse] = notImplemented()
  
  def createElection(ctx: RequestContext, electionId : Long) : Future[HttpResponse] = notImplemented()
}