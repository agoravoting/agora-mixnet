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
//import scala.reflect.runtime.universe._
import scala.util.{Try, Success, Failure}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.Queue
import java.util.concurrent.atomic.AtomicInteger
import akka.http.scaladsl.model._

trait GetType {  
  def getElectionTypeCreated[W <: Nat : ToInt] (election: Election[W, Created]) : String = {
    s"app.Election[shapeless.nat._${toInt[W]},app.Created]"
  }
  
  def getElectionCreated[W <: Nat : ToInt]() : String = {
    s"app.Election[shapeless.nat._${toInt[W]},app.Created]"
  }
  
  def getElectionTypeShares[W <: Nat : ToInt, T <: Nat : ToInt] (election: Election[W, Shares[T]]) : String = {
    s"app.Election[shapeless.nat._${toInt[W]},app.Shares[shapeless.nat._${toInt[T]}]]"
  }
  
  def getElectionShares[W <: Nat : ToInt, T <: Nat : ToInt]() : String = {
    s"app.Election[shapeless.nat._${toInt[W]},app.Shares[shapeless.nat._${toInt[T]}]]"
  }
  
  def getElectionTypeCombined[W <: Nat : ToInt] (election: Election[W, Combined]) : String = {
    s"app.Election[shapeless.nat._${toInt[W]},app.Combined]"
  }
  
  def getElectionCombined[W <: Nat : ToInt]() : String = {
    s"app.Election[shapeless.nat._${toInt[W]},app.Combined]"
  }
  
  def getElectionTypeVotes[W <: Nat : ToInt] (election: Election[W, Votes]) : String = {
    s"app.Election[shapeless.nat._${toInt[W]},app.Votes[${election.state.votes.length}]]"
  }
  
  def getElectionVotes[W <: Nat : ToInt](numVotes: Int) : String = {
    s"app.Election[shapeless.nat._${toInt[W]},app.Votes[${numVotes}]]"
  }
  
  def getElectionTypeVotesStopped[W <: Nat : ToInt] (election: Election[W, VotesStopped]) : String = {
    s"app.Election[shapeless.nat._${toInt[W]},app.VotesStopped]"
  }
  
  def getElectionVotesStopped[W <: Nat : ToInt]() : String = {
    s"app.Election[shapeless.nat._${toInt[W]},app.VotesStopped]"
  }
  
  def getElectionTypeMixing[W <: Nat : ToInt, T <: Nat : ToInt] (election: Election[W, Mixing[T]]) : String = {
    s"app.Election[shapeless.nat._${toInt[W]},app.Mixing[shapeless.nat._${toInt[T]}]]"
  }
  
  def getElectionMixing[W <: Nat : ToInt, T <: Nat : ToInt]() : String = {
    s"app.Election[shapeless.nat._${toInt[W]},app.Mixing[shapeless.nat._${toInt[T]}]]"
  }
  
  def getElectionTypeMixed[W <: Nat : ToInt] (election: Election[W, Mixed]) : String = {
    s"app.Election[shapeless.nat._${toInt[W]},app.Mixed]"
  }
  
  def getElectionMixed[W <: Nat : ToInt]() : String = {
    s"app.Election[shapeless.nat._${toInt[W]},app.Mixed]"
  }
  
  def getElectionTypeDecryptions[W <: Nat : ToInt, T <: Nat : ToInt] (election: Election[W, Decryptions[T]]) : String = {
    s"app.Election[shapeless.nat._${toInt[W]},app.Decryptions[shapeless.nat._${toInt[T]}]]"
  }
  
  def getElectionDecryptions[W <: Nat : ToInt, T <: Nat : ToInt]() : String = {
    s"app.Election[shapeless.nat._${toInt[W]},app.Decryptions[shapeless.nat._${toInt[T]}]]"
  }
  
  def getElectionTypeDecrypted[W <: Nat : ToInt] (election: Election[W, Decrypted]) : String = {
    s"app.Election[shapeless.nat._${toInt[W]},app.Decrypted]"
  }
  
  def getElectionDecrypted[W <: Nat : ToInt]() : String = {
    s"app.Election[shapeless.nat._${toInt[W]},app.Decrypted]"
  }
}

class ElectionSubscriber[W <: Nat : ToInt](val uid : String) extends GetType {
  println("GG ElectionSubscriber:constructor")
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
  
  def push[B <: ElectionState](election : Election[W, B], electionType: String) = {
    println("GG ElectionSubscriber:push electionType " + electionType)
    val promise = Promise[Election[W, B]]()
    val any = getOrAdd(electionType, promise)
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
  
  private def pull[B <: ElectionState](electionType: String): Future[Election[W, B]] = {
    println("GG ElectionSubscriber:pull electionType " + electionType)
    val promise = Promise[Election[W, B]]()
    val any = getOrAdd(electionType, promise)
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
    pull[Created](getElectionCreated[W])
  
  def startShares() : Future[Election[W, Shares[_0]]] = 
    pull[Shares[_0]](getElectionShares[W, _0])
  
  def addShare[T <: Nat : ToInt](): Future[Election[W, Shares[T]]] =
    pull[Shares[T]](getElectionShares[W, T])
    
  def combineShares() : Future[Election[W, Combined]]  = 
    pull[Combined](getElectionCombined[W])
    
  def startVotes() : Future[Election[W, Votes]] =
    pull[Votes](getElectionVotes[W](0))
    
  def addVote(numVotes: Int) : Future[Election[W, Votes]]  = 
    pull[Votes](getElectionVotes[W](numVotes))
  
  def addVotes(numVotes: Int) : Future[Election[W, Votes]]  = 
    pull[Votes](getElectionVotes[W](numVotes))
  
  def stopVotes() : Future[Election[W, VotesStopped]]  = 
    pull[VotesStopped](getElectionVotesStopped[W])
  
  def startMixing() : Future[Election[W, Mixing[_0]]]  = 
    pull[Mixing[_0]](getElectionMixing[W, _0])
  
  def addMix[T <: Nat : ToInt](): Future[Election[W, Mixing[Succ[T]]]]  = 
    pull[Mixing[Succ[T]]](getElectionMixing[W, T])
  
  def stopMixing() : Future[Election[W, Mixed]]  = 
    pull[Mixed](getElectionMixed[W])
  
  def startDecryptions() : Future[Election[W, Decryptions[_0]]]  = 
    pull[Decryptions[_0]](getElectionDecryptions[W, _0])
  
  def addDecryption[T <: Nat : ToInt](): Future[Election[W, Decryptions[Succ[T]]]]  = 
    pull[ Decryptions[Succ[T]]](getElectionDecryptions[W, T])
  
  def combineDecryptions() : Future[Election[W, Decrypted]]  = 
    pull[Decrypted](getElectionDecrypted[W])
}

class ElectionStateMaintainer[W <: Nat : ToInt](val uid : String)
  extends ElectionJsonFormatter
  with GetType
{
  println("GG ElectionStateMaintainer:constructor")
  private val subscriber = new ElectionSubscriber[W](uid)
  
  def startShares(in: Election[W, Created]) : Election[W, Shares[_0]] = {
      println("GG ElectionStateMaintainer::startShares")
      new Election[W, Shares[_0]](Shares[_0](List[(String, String)]().sized(0).get, in.state))
  }
  
  def addShare[T <: Nat : ToInt](in: Election[W, Shares[T]], proverId: String, keyShare: String) : Election[W, Shares[Succ[T]]] = {
    println(s"GG ElectionStateMaintainer::addShare")
    new Election[W, Shares[Succ[T]]](Shares[Succ[T]](in.state.shares :+ (proverId, keyShare), in.state))
  }
  
  def combineShares(in: Election[W, Shares[W]], publicKey: String) : Election[W, Combined] = {
    println(s"GG ElectionStateMaintainer::combineShares")
    new Election[W, Combined](Combined(publicKey, in.state))
  }
  
  def pushCombined(jsCombined: JsCombined) {
    println("GG ElectionStateMaintainer::pushCombined")
    val futureShare = subscriber.addShare[W]()
    futureShare onComplete {
      case Success(share) => 
        val election = combineShares(share, jsCombined.publicKey)
        subscriber.push(election, getElectionTypeCombined(election))
      case Failure(err) =>
        println(s"Future error: ${err}")
    }
    
  }
  
  def pushShares(jsShares: JsShares) {
    println("GG ElectionStateMaintainer:pushShares")
    val maxLevel = ToInt[W].apply()
    if(jsShares.level == 0) {
      val futureCreate = subscriber.create()
      futureCreate onComplete {
        case Success(cc) =>
          val election = startShares(cc)
          subscriber.push(election, getElectionTypeShares(election))
        case Failure(e) =>
          println(s"Future error: ${e}")
      }
    } else if (jsShares.level > 0 && jsShares.level <= maxLevel) {
      jsShares.level match {
        case 1 => 
          val futureShare = subscriber.startShares()
          futureShare onComplete { 
            case Success(share) =>
              val election = addShare(share, jsShares.shares._1, jsShares.shares._2)
              subscriber.push(election, getElectionTypeShares(election))
            case Failure(e) => 
              println(s"Future error: ${e}")
          }
        case 2 => 
          val futureShare = subscriber.addShare[_1]()
          futureShare onComplete { 
            case Success(share) =>
              val election = addShare(share, jsShares.shares._1, jsShares.shares._2)
              subscriber.push(election, getElectionTypeShares(election))
            case Failure(e) => 
              println(s"Future error: ${e}")
          }
        case 3 => 
          val futureShare = subscriber.addShare[_2]()
          futureShare onComplete { 
            case Success(share) =>
              val election = addShare(share, jsShares.shares._1, jsShares.shares._2)
              subscriber.push(election, getElectionTypeShares(election))
            case Failure(e) => 
              println(s"Future error: ${e}")
          }
        case 4 => 
          val futureShare = subscriber.addShare[_3]()
          futureShare onComplete { 
            case Success(share) =>
              val election = addShare(share, jsShares.shares._1, jsShares.shares._2)
              subscriber.push(election, getElectionTypeShares(election))
            case Failure(e) => 
              println(s"Future error: ${e}")
          }
        case 5 => 
          val futureShare = subscriber.addShare[_4]()
          futureShare onComplete { 
            case Success(share) =>
              val election = addShare(share, jsShares.shares._1, jsShares.shares._2)
              subscriber.push(election, getElectionTypeShares(election))
            case Failure(e) => 
              println(s"Future error: ${e}")
          }
        case 6 => 
          val futureShare = subscriber.addShare[_5]()
          futureShare onComplete { 
            case Success(share) =>
              val election = addShare(share, jsShares.shares._1, jsShares.shares._2)
              subscriber.push(election, getElectionTypeShares(election))
            case Failure(e) => 
              println(s"Future error: ${e}")
          }
        case 7 => 
          val futureShare = subscriber.addShare[_6]()
          futureShare onComplete { 
            case Success(share) =>
              val election = addShare(share, jsShares.shares._1, jsShares.shares._2)
              subscriber.push(election, getElectionTypeShares(election))
            case Failure(e) => 
              println(s"Future error: ${e}")
          }
        case 8 => 
          val futureShare = subscriber.addShare[_7]()
          futureShare onComplete { 
            case Success(share) =>
              val election = addShare(share, jsShares.shares._1, jsShares.shares._2)
              subscriber.push(election, getElectionTypeShares(election))
            case Failure(e) => 
              println(s"Future error: ${e}")
          }
        case _ => 
          val futureShare = subscriber.addShare[_8]()
          futureShare onComplete { 
            case Success(share) =>
              val election = addShare(share, jsShares.shares._1, jsShares.shares._2)
              subscriber.push(election, getElectionTypeShares(election))
            case Failure(e) => 
              println(s"Future error: ${e}")
          }
      }
    } else {
      println("Error, mismatched levels")
    }
  }
  
  def pushCreate(jsElection: JsElection, uid: String) {
    val group = GStarModSafePrime.getInstance(new BigInteger(jsElection.state.cSettings.group))
    val cSettings = CryptoSettings(group, group.getDefaultGenerator())
    if (jsElection.level == ToInt[W].apply()) {
      val election = 
        new Election[W, Created](Created(jsElection.state.id, cSettings, uid))
      subscriber.push(election, getElectionTypeCreated(election))
    } else {
      println("Error, mismatched levels")
    }
  }
  
  def push(post: Post) {
    println("GG ElectionStateMaintainer:push")
    val jsMsg = Json.parse(post.message)
    if(post.user_attributes.section == "election" &&
         post.user_attributes.group == "create") {
        jsMsg.validate[JsElection] match {
          case jSeqPost: JsSuccess[JsElection] =>
            pushCreate(jSeqPost.get, post.board_attributes.index)
          case e: JsError => 
            println(s"\ElectionStateMaintainer JsError error: ${e} message ${post.message}")
        }
      } else if (post.user_attributes.section == "election" &&
         post.user_attributes.group == uid) {
        jsMsg.validate[JsMessage] match {
          case a: JsSuccess[JsMessage] =>
            val jsMessage = a.get
            jsMessage.messageType match {
              case "Shares" =>
                jsMessage.message.validate[JsShares] match {
                  case b: JsSuccess[JsShares] =>
                    pushShares(b.get)
                  case e: JsError =>
                    println(s"JsError error: ${e} message ${post.message}")
                }
              case "Combined" =>
                jsMessage.message.validate[JsCombined] match {
                  case b: JsSuccess[JsCombined] =>
                    pushCombined(b.get)
                  case e: JsError =>
                    println(s"JsError error: ${e} message ${post.message}")
                }
              case _ => ;
                println(s"ElectionStateMaintainer JsMessage type error: ${jsMessage.messageType}")
            }
          case e: JsError => 
            println(s"ElectionStateMaintainer error: ${e} message ${post.message}")
        }
      } else {
            println("ElectionStateMaintainer else")
      }
  }
  
  def getSubscriber() : ElectionSubscriber[W] = {
    subscriber
  }
}

class MaintainerWrapper(level: Int, uid: String) {
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
                /*val messageB64 = post.message.replace('.', '=')
                val message = new String(Base64.getDecoder.decode(messageB64), StandardCharsets.UTF_8)*/
                val jsMsg = Json.parse(post.message)
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
    println("GG PostOffice::remove")
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