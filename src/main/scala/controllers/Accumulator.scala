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
import scala.util.{Success, Failure}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.Queue
import java.util.concurrent.atomic.AtomicInteger

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
    val any = getOrAdd(str, promise)
    val p = any.asInstanceOf[Promise[Election[W, B]]]  // TODO: This could fail
    if (p.isCompleted) {
      println("Error: trying to complete an already completed future")
    } else {
      p.success(election) 
    }
  }
  
  private def pull[B <: ElectionState : TypeTag](): Future[Election[W, B]] = {
    val promise = Promise[Election[W, B]]()
    val str = getElectionType(promise)
    val any = getOrAdd(str, promise)
    val p = any.asInstanceOf[Promise[Election[W, B]]]  // TODO: This could fail
    promise.future
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
  
  
  val subscriber = new ElectionSubscriber[W](uid)  
  
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
}

/*case class Caster[T <: Nat : ToInt]() {
  def whatever() : String = {
    "hola"
  }
  
  type TheType = T
}*/

trait PostOffice {
  // post index counter
  private var index = 0
  private var queue = Queue[Option[Post]]()
  def add(post: Post) {
    queue.synchronized {
      val postIndex = post.board_attributes.index.toInt
      if(postIndex < index) {
        println("Error: old post")
      } else if(postIndex >= index) {
        if(postIndex < index + queue.size) {
          queue.get(postIndex - index) map { x =>
            x match {
              case Some(p) =>
                println("Error: duplicated post")
              case None =>
                queue.update(postIndex - index, Some(post))
            }
          } 
        } else {
          queue ++= List.fill(postIndex - (index + queue.size))(None)
          queue += Some(post)
        }
      }
    }
    remove()
  }
  
  def send(post: Post)
  
  
  private def remove() {
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
  
}

object BoardReader
  extends ElectionJsonFormatter
  with PostOffice
{  
  def send(post: Post) {
  }
  
  def push(seqPost: Seq[Post]) = {
    seqPost foreach { post => 
      add(post)
    }
  }
  
  
}