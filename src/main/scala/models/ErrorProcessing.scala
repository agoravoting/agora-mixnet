package models


import play.api.libs.ws._
import scala.util.{Try, Success, Failure}
import play.api.libs.json._

trait ErrorProcessing {
  /**
   * Get the message safely from a `Throwable`
   */
  def getMessageFromThrowable(t: Throwable): String = {
    if (null == t.getCause) {
        t.toString
     } else {
        t.getCause.getMessage
     }
  }
}