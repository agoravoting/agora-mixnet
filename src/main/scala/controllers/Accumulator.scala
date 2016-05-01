package controllers

import shapeless._
import syntax.typeable._
import nat._
import syntax.sized._
import ops.nat._
import LT._

import ops.{ hlist, coproduct }
import scala.language.experimental.macros
import scala.annotation.{ StaticAnnotation, tailrec }
import scala.reflect.macros.{ blackbox, whitebox }

import app._
import models._
import java.util.Base64
import java.nio.charset.StandardCharsets
import play.api.libs.json._
import ch.bfh.unicrypt.math.algebra.multiplicative.classes.GStarModSafePrime
import java.math.BigInteger
import scala.concurrent.{Future, Promise}
import scala.reflect.runtime.universe._
import scala.util.{Try, Success, Failure}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.Queue
import java.util.concurrent.atomic.AtomicInteger
import akka.http.scaladsl.model._

class ElectionSubscriber[W <: Nat : ToInt : TypeTag](val uid : String){
  private var map = Map[String, Any]()
  
  private def getOrAdd(key: String, value: Any) : Any = {
    map.synchronized {
      map.get(key) match {
        case Some(any) =>
          any
        case None =>
          map += (key -> value)
          value
      }
    }
  }
  
  private def getElectionType[S <: ElectionState : TypeTag](x: Promise[Election[W, S]]) : String = {
    typeOf[Promise[Election[W, S]]].toString
  }
  
  def push[B <: ElectionState : TypeTag](election : Election[W, B]) = {
    val promise = Promise[Election[W, B]]()
    val str = getElectionType(promise)
    println(s"GG ElectionSubscriber:push $str")
    val any = getOrAdd(str, promise)
    Try {
      any.asInstanceOf[Promise[Election[W, B]]]
    } map { p =>
      if (p.isCompleted) {
        println("Error: trying to complete an already completed future")
      } else {
        p.success(election)  
      }
    }
  }
  
  private def pull[B <: ElectionState : TypeTag](): Future[Election[W, B]] = {
    println(s"GG ElectionSubscriber:pull")
    val promise = Promise[Election[W, B]]()
    val str = getElectionType(promise)
    val any = getOrAdd(str, promise)
    Try {
      any.asInstanceOf[Promise[Election[W, B]]]
    } match {
      case Success(p) =>
        p.future
      case Failure(e) =>
        promise.failure(e)
        promise.future
    }
  }
  
  def create() : Future[Election[W, Created]] = 
    pull[Created]()
  
  def startShares() : Future[Election[W, Shares[_0]]] =
    pull[Shares[_0]]()
  
  def addShare[T <: Nat : TypeTag]() : Future[Election[W, Shares[T]]] =
    pull[Shares[T]]()
    
  def combineShares() : Future[Election[W, Combined]]  = 
    pull[Combined]()
    
  def startVotes() : Future[Election[W, Votes]] =
    pull[Votes]()
    
  def addVote() : Future[Election[W, Votes]]  = 
    pull[Votes]()
  
  def addVotes() : Future[Election[W, Votes]]  = 
    pull[Votes]()
  
  def stopVotes() : Future[Election[W, VotesStopped]]  = 
    pull[VotesStopped]()
  
  def startMixing() : Future[Election[W, Mixing[_0]]]  = 
    pull[Mixing[_0]]()
  
  def addMix[T <: Nat : TypeTag](): Future[Election[W, Mixing[Succ[T]]]]  = 
    pull[Mixing[Succ[T]]]()
  
  def stopMixing() : Future[Election[W, Mixed]]  = 
    pull[Mixed]()
  
  def startDecryptions() : Future[Election[W, Decryptions[_0]]]  = 
    pull[Decryptions[_0]]()
  
  def addDecryption[T <: Nat : TypeTag](): Future[Election[W, Decryptions[Succ[T]]]]  = 
    pull[ Decryptions[Succ[T]]]()
  
  def combineDecryptions() : Future[Election[W, Decrypted]]  = 
    pull[Decrypted]()
}

class ElectionStateMaintainer[W <: Nat : ToInt : TypeTag](val uid : String)
  extends ElectionJsonFormatter
{  
  def startShares(in: Election[W, Created]) : Election[W, Shares[_0]] = {
      println("Now waiting for shares")
      new Election[W, Shares[_0]](Shares[_0](List[(String, String)]().sized(0).get, in.state))
  }
  
  def addShare[T <: Nat](in: Election[W, Shares[T]], keyShare: String, proverId: String) : Election[W, Shares[Succ[T]]] = {
    println(s"Adding share...")
    new Election[W, Shares[Succ[T]]](Shares[Succ[T]](in.state.shares :+ (proverId, keyShare), in.state))
  }
  
  private val subscriber = new ElectionSubscriber[W](uid)
  
  def pushShares(jsShares: JsShares) {
    val maxLevel = ToInt[W].apply()
    if(jsShares.level == 0) {
      val futureCreate = subscriber.create()
      futureCreate onComplete {
        case Success(cc) =>
          val election = startShares(cc)
          subscriber.push(election)
        case Failure(e) =>
          println(s"Future error: ${e}")
      }
    } else if (jsShares.level > 0 && jsShares.level <= maxLevel) {
      jsShares.level match {
        case 1 => 
          val futureShare = subscriber.startShares()
          futureShare onComplete { 
            case Success(share) =>
              val election = addShare(share, jsShares.shares._2, jsShares.shares._1)
              subscriber.push(election)
            case Failure(e) => 
              println(s"Future error: ${e}")
          }
        case 2 => 
          val futureShare = subscriber.addShare[_1]()
          futureShare onComplete { 
            case Success(share) =>
              val election = addShare(share, jsShares.shares._2, jsShares.shares._1)
              subscriber.push(election)
            case Failure(e) => 
              println(s"Future error: ${e}")
          }
        case _ => 
          val futureShare = subscriber.addShare[_1]()
          futureShare onComplete { 
            case Success(share) =>
              val election = addShare(share, jsShares.shares._2, jsShares.shares._1)
              subscriber.push(election)
            case Failure(e) => 
              println(s"Future error: ${e}")
          }
      }
    } else {
      println("Error, mismatched levels")
    }
  }
  
  def pushCreate(jsElection: JsElection) {
    println("GG ElectionStateMaintainer:pushCreate")
    val group = GStarModSafePrime.getInstance(new BigInteger(jsElection.state.cSettings.group))
    val cSettings = CryptoSettings(group, group.getDefaultGenerator())
    if (jsElection.level == ToInt[W].apply()) {
      val election = 
        new Election[W, Created](Created(jsElection.state.id, cSettings, jsElection.state.uid))
      subscriber.push(election)
    } else {
      println("Error, mismatched levels")
    }
  }
  
  def push(post: Post) {
    println("GG ElectionStateMaintainer:push")
    val messageB64 = post.message.replace('.', '=')
    val message = new String(Base64.getDecoder.decode(messageB64), StandardCharsets.UTF_8)
    val jsMsg = Json.parse(message)
    if(post.user_attributes.section == "election" &&
         post.user_attributes.group == "create") {
        jsMsg.validate[JsElection] match {
          case jSeqPost: JsSuccess[JsElection] =>
            pushCreate(jSeqPost.get)            
          case e: JsError => 
            println(s"\ElectionStateMaintainer 1 JsError error: ${e} message ${message}")
        }
      } else if (post.user_attributes.section == "election" &&
         post.user_attributes.group == uid) {
        jsMsg.validate[JsMessage] match {
          case a: JsSuccess[JsMessage] =>
            val jsMessage = a.get
            jsMessage.messageType match {
              case "JsShares" =>
                jsMessage.message.validate[JsShares] match {
                  case b: JsSuccess[JsShares] =>
                    pushShares(b.get)
                  case e: JsError =>
                    println(s"JsError error: ${e} message ${message}")
                }
              case _ => ;
                println(s"ElectionStateMaintainer JsMessage type error: ${jsMessage.messageType}")
            }
          case e: JsError => 
            println(s"ElectionStateMaintainer error: ${e} message ${message}")
        }
        val futureShare = subscriber.addShare[_1]()
      } else {
            println("ElectionStateMaintainer else")
      }
  }
  
  def getSubscriber() : ElectionSubscriber[W] = {
    subscriber
  }
}

class MaintainerWrapper(level: Int, uid: String) {
  println("GG MaintainerWrapper:constructor")
  
  val maintainer =  if(1 == level) {
    new ElectionStateMaintainer[_1](uid)
  } else if(2 == level) {
    new ElectionStateMaintainer[_2](uid)
  } else if(3 == level) {
    new ElectionStateMaintainer[_3](uid)
  } else if(4 == level) {
    new ElectionStateMaintainer[_4](uid)
  } else if(5 == level) {
    new ElectionStateMaintainer[_5](uid)
  } else if(6 == level) {
    new ElectionStateMaintainer[_6](uid)
  } else if(7 == level) {
    new ElectionStateMaintainer[_7](uid)
  } else if(8 == level) {
    new ElectionStateMaintainer[_8](uid)
  } else if(9 == level) {
    new ElectionStateMaintainer[_9](uid)
  } else {
    throw new Error(s"level is $level and should be limited to [1-9]")
  }
  
  def push(post: Post) {
    println("GG MaintainerWrapper:push")
    maintainer.push(post)
  }
  
  def getSubscriber() = {
    maintainer.getSubscriber()
  }
}

trait PostOffice extends ElectionJsonFormatter
{
  // post index counter
  private var index : Long = 0
  private var queue = Queue[Option[Post]]()
  // the first parameter is the uid
  private var electionMap = Map[Long, MaintainerWrapper]()
  // list of callbacks to be called when a new election is created
  private var callbackQueue = Queue[String => Unit]()
  
  def add(post: Post) {
    queue.synchronized {
      println("GG PostOffice:add")
      Try {
        post.board_attributes.index.toLong
      } map { postIndex =>
        if(postIndex < index) {
          println("Error: old post")
        } else if(postIndex >= index) {
          if(postIndex < index + queue.size) {
            queue.get((postIndex - index).toInt) map { x =>
              x match {
                case Some(p) =>
                  println("Error: duplicated post")
                case None =>
                  queue.update((postIndex - index).toInt, Some(post))
              }
            }
          } else {
            queue ++= List.fill((postIndex - (index + (queue.size).toLong)).toInt)(None)
            queue += Some(post)
          }
        }
      }
      remove()
    }
  }
  
  private def send(post: Post) {
    println("GG PostOffice:send")
    if("election" == post.user_attributes.section) {
      val group : String = post.user_attributes.group
      val electionIdStr = post.board_attributes.index
      if("create" == group) {
        Try { electionIdStr.toLong } match {
          case Success(electionId) =>
            electionMap.get(electionId) match {
              case Some(electionWrapper) =>
                println(s"Error: duplicated Election Id: ${electionId}")
              case None =>
                val messageB64 = post.message.replace('.', '=')
                val message = new String(Base64.getDecoder.decode(messageB64), StandardCharsets.UTF_8)
                val jsMsg = Json.parse(message)
                jsMsg.validate[JsElection] match {
                  case jSeqPost: JsSuccess[JsElection] =>
                    val maintainer = new MaintainerWrapper(jSeqPost.get.level, electionIdStr)
                    maintainer.push(post)
                    electionMap += (electionId -> maintainer)
                    callbackQueue.synchronized {
                      callbackQueue foreach { func =>
                        func(electionIdStr)
                      }
                    }
                  case e: JsError => 
                    println("Error: JsCreate format error")
                }
            }
          case Failure(e) =>
            println(s"Error: Election Id is not a number (but It should be): ${electionIdStr}")
        }
      } else {
        Try { group.toLong } match {
          case Success(electionId) => 
            electionMap.get(electionId) match {
              case Some(electionWrapper) => 
                electionWrapper.push(post)
              case None =>
                println(s"Error: Election Id not found in db: ${electionId}")
            }
          case Failure(e) => 
            println(s"Error: group is not a number : ${group}")
        }
      }
    } else {
      println("Error: post is not an election")
    }
  }
  
  private def remove() {
    println("GG PostOffice:remove")
    var head : Option[Post] = None
    queue.synchronized {
      if(queue.size > 0) {
        queue.head match {
          case Some(post) =>
            // TODO: here we should check the post hash and signature
            head = queue.dequeue
            index = index + 1
          case None => ;
        }
      }
    }
    head match {
      case Some(post) =>
        send(post)
      case None => ;
    }
  }
  
  def getSubscriber(uid : String) = {
    Try { uid.toLong } match {
      case Success(electionId) =>
        electionMap.get(electionId) match {
          case Some(electionWrapper) => 
            electionWrapper.getSubscriber()
          case None =>
            throw new Error(s"Error subscribing: Election Id not found in db: ${electionId}")
        }
      case Failure(e) =>
        throw new Error(s"Error subscribing: Election id is not a number: {uid}")
    }
  }
  
  def addElectionCreationListener(callback: (String) => Unit) {
    callbackQueue.synchronized {
      callbackQueue += callback
    }
  }
}


object BoardReader
  extends ElectionJsonFormatter
  with PostOffice
  with FiwareJSONFormatter
  with BoardJSONFormatter
{  
  def accumulate(bodyStr: String) : Future[HttpResponse] = {
    val promise = Promise[HttpResponse]()
    println(s"Router accumulate: $bodyStr")
    val json = Json.parse(bodyStr)
    json.validate[AccumulateRequest] match {
      case sr: JsSuccess[AccumulateRequest] =>
        var jsonError: Option[String] = None
        val postSeq = sr.get.contextResponses flatMap {  x => 
          x.contextElement.attributes flatMap { y =>
            y.value.validate[Post] match {
              case post: JsSuccess[Post] =>
                Some(post.get)
              case e: JsError =>
                val str = "processAccumulate has a None: this is not " +
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
         promise.success(HttpResponse(entity = s"OK"))
     }
     case e: JsError =>
       val errorText = s"Bad request: invalid AccumulateRequest json: $bodyStr\nerror: ${e}\n"
         println(errorText)
         promise.success(HttpResponse(400, entity = errorText))
    }
    promise.future
  }
  
  def push(seqPost: Seq[Post]) = {
    seqPost foreach { post => 
      add(post)
    }
  }
  
  
}