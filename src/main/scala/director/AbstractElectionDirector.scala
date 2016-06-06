package director

import akka.http.scaladsl.server.RequestContext
import scala.concurrent.Future
import akka.http.scaladsl.model._

trait AbstractElectionDirector {
  def addVote(ctx: RequestContext, electionId : Long, voterId: String) : Future[HttpResponse]
  def createElection(ctx: RequestContext, electionId : Long) : Future[HttpResponse]
}