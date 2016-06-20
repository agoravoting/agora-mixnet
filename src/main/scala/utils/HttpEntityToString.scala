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

package utils

import scala.concurrent.{Future, Promise}
import akka.stream.ActorMaterializer
import akka.http.scaladsl.model._
import scala.concurrent.duration._

trait HttpEntityToString {
  import scala.concurrent.ExecutionContext
  
  def getString(entity: HttpEntity) (implicit ec: ExecutionContext, fm: ActorMaterializer): Future[String] =  {
    val promise = Promise[String]()
    Future {
      val future = entity.toStrict(5.seconds) map { st =>
        st.data.decodeString("UTF-8")
      }
      promise.completeWith(future)
    }
    promise.future
  }
}