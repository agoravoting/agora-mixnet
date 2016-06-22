/**
 * This file is part of agora-mixnet.
 * Copyright (C) 2015-2016  Agora Voting SL <agora@agoravoting.com>

 * agora-mixnet is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License.

 * agora-mixnet is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.

 * You should have received a copy of the GNU Affero General Public License
 * along with agora-mixnet.  If not, see <http://www.gnu.org/licenses/>.
**/

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
  
  def stopElection(ctx: RequestContext, electionId : Long) : Future[HttpResponse] = notImplemented()
}