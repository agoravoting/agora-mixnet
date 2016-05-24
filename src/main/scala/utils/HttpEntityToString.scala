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