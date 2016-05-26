package utils

import scala.concurrent._
import akka.http.scaladsl.model.HttpRequest

/** Authorizes requests using hmac in Authorization header */
case class HMACAuthCheck(userId: String, objType: String, objId: Long, perm: String) (implicit ec: ExecutionContext) {

  val boothSecret = BoardConfig.booth.auth.secret
  val expiry = BoardConfig.booth.auth.expiry

  /** deny requests that dont pass hmac validations */
  def check(request: HttpRequest) : Future[Boolean] = {
    val promise = Promise[Boolean]()
    Future {
      val authHeaderOpt = request.getHeader("Authorization")
      if(!authHeaderOpt.isPresent()) {
        promise.failure(new Error("authorization header not present!"))
      } else {
        val authHeader = authHeaderOpt.get()
        promise.success{ 
          blocking {
            validate(authHeader.value())
          }
        }
      }
    } recover { case err =>
      promise.tryFailure(err)
    }
    promise.future
  }

  /** validate an hmac authorization code

   Format is: "khmac://sha-256;<hash>/<message>"

   Format of the message is:
   "<userid:String>:<obj_type:String>:<obj_id:Long>:<perm:String>:<time:Long>"
   */
  def validate(value: String): Boolean = {
    try {
      val start = "khmac:///sha-256;";
      val slashPos = start.length + 64;

      if(!value.startsWith(start) || value.length < slashPos || value.charAt(slashPos) != '/') {
        println(s"Malformed authorization header")
        return false
      }
      val hash = value.substring(start.length, slashPos)
      val message = value.substring(slashPos + 1)

      val split = message.split(':')
      if(split.length != 5) {
        println(s"Malformed authorization header")
        return false
      }

      val rcvUserId = split(0)
      val rcvObjType = split(1)
      val rcvObjId = split(2).toLong
      val rcvPerm = split(3)
      val rcvTime = split(4).toLong
      val now = new java.util.Date().getTime / 1000
      val diff = now - rcvTime

      val compareOk = (Crypto.hmac(boothSecret, message) == hash)//PlayCrypto.constantTimeEquals(Crypto.hmac(boothSecret, message), hash)

      // Logger.info(Crypto.hmac(boothSecret, message))
      // Logger.info(hash)
      // Logger.info(compareOk + " " + (diff < expiry) + " " + (rcvUserId == userId) + " " + (rcvObjType == objType) + " " + (rcvObjId == objId) + " " + (rcvPerm == perm))

      // if the userId is the empty string we don't mind the user
      val userOk = (rcvUserId == userId || userId == "")

      // note that we can compare without doing contant time comparison received
      // strings because that's not critical for security, only hmac is
      if(compareOk && (diff < expiry) && userOk && (rcvObjType == objType) &&
        (rcvObjId == objId) && (rcvPerm == perm)) {

        return true
      }

      println(s"Failed to authorize hmac ($value) $compareOk $diff $expiry $userOk $rcvObjType $objType $rcvObjId $objId $rcvPerm $perm")
      return false
    }
    catch {
      case e:Exception => println(s"Exception verifying hmac ($value)", e); false
    }
  }
}