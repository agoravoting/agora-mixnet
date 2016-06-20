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

package accumulator

import shapeless._
import nat._
import syntax.sized._
import ops.nat._
import play.api.libs.json._
import ch.bfh.unicrypt.math.algebra.multiplicative.classes.GStarModSafePrime
import java.math.BigInteger
import scala.util.{Try, Success, Failure}
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import election._
import models._

class ElectionStateMaintainer[W <: Nat : ToInt](val uid : String)
  extends ElectionJsonFormatter
  with GetType
  with ErrorProcessing
{
  implicit val system = ActorSystem()
  implicit val executor = system.dispatchers.lookup("my-other-dispatcher")
  implicit val materializer = ActorMaterializer()
  println("GG ElectionStateMaintainer::constructor")
  private val subscriber = new ElectionSubscriber[W](uid)
  private val dto = new ElectionDTOData(uid.toLong, toInt[W])
  private var rawResults: Option[String] = None
  
  def getElectionInfo() : ElectionDTO = {
    dto()
  }
  
  def getResults() : Option[String] = {
    rawResults
  }
  
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
  
  def startVotes(in: Election[W, Combined]) : Election[W, Votes] = {
    println(s"GG ElectionStateMaintainer::startVotes")
    new Election[W, Votes](Votes(List[String](), 0, in.state))
  }
  
  def addVotes(in: Election[W, Votes], votes : List[String]) : Election[W, Votes] = {
    println(s"GG ElectionStateMaintainer::addVotes")
    new Election[W, Votes](Votes(votes ::: in.state.votes, in.state.addVoteIndex + 1, in.state))
  }
  
  def stopVotes(in: Election[W, Votes], lastAddVoteIndex: Int, date: com.github.nscala_time.time.Imports.DateTime) : Election[W, VotesStopped] = {
    println(s"GG ElectionStateMaintainer::stopVotes")
    new Election[W, VotesStopped](VotesStopped(lastAddVoteIndex, in.state, date))
  }
  
  def startMixing(in: Election[W, VotesStopped]) : Election[W, Mixing[_0]] = {
    println(s"GG ElectionStateMaintainer::startMixing")
    new Election[W, Mixing[_0]](Mixing[_0](List[ShuffleResultDTO]().sized(0).get, in.state))
  }
  
  def addMix[T <: Nat : ToInt](in: Election[W, Mixing[T]], mix: ShuffleResultDTO) : Election[W, Mixing[Succ[T]]] = {
    println(s"GG ElectionStateMaintainer::addMix")
    new Election[W, Mixing[Succ[T]]](Mixing[Succ[T]](in.state.mixes :+ mix, in.state))
  }
  
  def stopMixing(in: Election[W, Mixing[W]]) : Election[W, Mixed] = {
    println(s"GG ElectionStateMaintainer::stopMixing")
    new Election[W, Mixed](Mixed(in.state))
  }
  
  def startDecryptions(in: Election[W, Mixed]) : Election[W, Decryptions[_0]] = {
    println(s"GG ElectionStateMaintainer::startDecryptions")
    new Election[W, Decryptions[_0]](Decryptions[_0](List[PartialDecryptionDTO]().sized(0).get, in.state))
  }
  
  def addDecryption[T <: Nat : ToInt](in: Election[W, Decryptions[T]], decryption: PartialDecryptionDTO) : Election[W, Decryptions[Succ[T]]] = {
    println(s"GG ElectionStateMaintainer::addDecryption")
    new Election[W, Decryptions[Succ[T]]](Decryptions[Succ[T]](in.state.decryptions :+ decryption, in.state))
  }
  
  def combineDecryptions(in: Election[W, Decryptions[W]], decrypted: Seq[String]) : Election[W, Decrypted] = {
    println(s"GG ElectionStateMaintainer::combineDecryptions")
    new Election[W, Decrypted](Decrypted(decrypted, in.state))
  }
  
  def pushDecrypted(jsDecrypted: JsDecrypted) = {
    println("GG ElectionStateMaintainer::pushDecrypted")
    val futureDecryption = subscriber.addDecryption[W]()
    futureDecryption onComplete {
      case Success(decryption) =>
        val election = combineDecryptions(decryption, jsDecrypted.decrypted)
        subscriber.push(election, getElectionTypeDecrypted(election))
        
        Tally.getRawResults(election.state.decrypted) map { raw =>
          rawResults = Some(raw)
        }
        Tally.tally(election, dto()) map { tallyStr =>
          dto.setResults(tallyStr)
          dto.setState(ElectionDTOData.RESULTS_PUB)
        }
      case Failure(err) =>
        println(s"Future error: ${getMessageFromThrowable(err)}")
    }
  }
  
  def pushDecryptions(jsDecryptions: JsDecryptions) = {
    println("GG ElectionStateMaintainer::pushDecryptions")
    if(jsDecryptions.level < 0 || jsDecryptions.level > 9) {
      println(s"Error, mismatched level (should be 1 to 9): ${jsDecryptions.level}")
    } else {
      jsDecryptions.level match {
        case 1 =>
          val futureDecryption = subscriber.addDecryption[_0]()
          futureDecryption onComplete {
            case Success(decryption) =>
              val election = addDecryption(decryption, jsDecryptions.decryption)
              subscriber.push(election, getElectionTypeDecryptions(election))
            case Failure(err) =>
              println(s"Future error: ${getMessageFromThrowable(err)}")
          }
        case 2 =>
          val futureDecryption = subscriber.addDecryption[_1]()
          futureDecryption onComplete {
            case Success(decryption) =>
              val election = addDecryption(decryption, jsDecryptions.decryption)
              subscriber.push(election, getElectionTypeDecryptions(election))
            case Failure(err) =>
              println(s"Future error: ${getMessageFromThrowable(err)}")
          }
        case 3 =>
          val futureDecryption = subscriber.addDecryption[_2]()
          futureDecryption onComplete {
            case Success(decryption) =>
              val election = addDecryption(decryption, jsDecryptions.decryption)
              subscriber.push(election, getElectionTypeDecryptions(election))
            case Failure(err) =>
              println(s"Future error: ${getMessageFromThrowable(err)}")
          }
        case 4 =>
          val futureDecryption = subscriber.addDecryption[_3]()
          futureDecryption onComplete {
            case Success(decryption) =>
              val election = addDecryption(decryption, jsDecryptions.decryption)
              subscriber.push(election, getElectionTypeDecryptions(election))
            case Failure(err) =>
              println(s"Future error: ${getMessageFromThrowable(err)}")
          }
        case 5 =>
          val futureDecryption = subscriber.addDecryption[_4]()
          futureDecryption onComplete {
            case Success(decryption) =>
              val election = addDecryption(decryption, jsDecryptions.decryption)
              subscriber.push(election, getElectionTypeDecryptions(election))
            case Failure(err) =>
              println(s"Future error: ${getMessageFromThrowable(err)}")
          }
        case 6 =>
          val futureDecryption = subscriber.addDecryption[_5]()
          futureDecryption onComplete {
            case Success(decryption) =>
              val election = addDecryption(decryption, jsDecryptions.decryption)
              subscriber.push(election, getElectionTypeDecryptions(election))
            case Failure(err) =>
              println(s"Future error: ${getMessageFromThrowable(err)}")
          }
        case 7 =>
          val futureDecryption = subscriber.addDecryption[_6]()
          futureDecryption onComplete {
            case Success(decryption) =>
              val election = addDecryption(decryption, jsDecryptions.decryption)
              subscriber.push(election, getElectionTypeDecryptions(election))
            case Failure(err) =>
              println(s"Future error: ${getMessageFromThrowable(err)}")
          }
        case 8 =>
          val futureDecryption = subscriber.addDecryption[_7]()
          futureDecryption onComplete {
            case Success(decryption) =>
              val election = addDecryption(decryption, jsDecryptions.decryption)
              subscriber.push(election, getElectionTypeDecryptions(election))
            case Failure(err) =>
              println(s"Future error: ${getMessageFromThrowable(err)}")
          }
        case _ =>
          val futureDecryption = subscriber.addDecryption[_8]()
          futureDecryption onComplete {
            case Success(decryption) =>
              val election = addDecryption(decryption, jsDecryptions.decryption)
              subscriber.push(election, getElectionTypeDecryptions(election))
            case Failure(err) =>
              println(s"Future error: ${getMessageFromThrowable(err)}")
          }
      }
    }
  }
  
  def pushStartDecryptions() {
    println("GG ElectionStateMaintainer::pushStartDecryptions")
    val futureStopMixing = subscriber.stopMixing()
    futureStopMixing onComplete {
      case Success(stop) =>
        val election = startDecryptions(stop)
        subscriber.push(election, getElectionTypeDecryptions(election))
      case Failure(err) =>
        println(s"Future error: ${getMessageFromThrowable(err)}")
    }
  }
  
  def pushStopMixing() {
    println("GG ElectionStateMaintainer::pushStopMixing")
    val futureMix = subscriber.addMix[W]()
    futureMix onComplete {
      case Success(mix) =>
        val election = stopMixing(mix)
        subscriber.push(election, getElectionTypeMixed(election))
      case Failure(err) =>
        println(s"Future error: ${getMessageFromThrowable(err)}")
    }
  }
  
  def pushMixing(jsMixing: JsMixing) {
    println("GG ElectionStateMaintainer::pushMixing")
    if(jsMixing.level < 1 || jsMixing.level > 9) {
      println(s"Error, mismatched level (should be 1 to 9): ${jsMixing.level}")
    } else {
      jsMixing.level match {
        case 1 =>
          val futureMix = subscriber.addMix[_0]()
          futureMix onComplete {
            case Success(mix) => 
              val election = addMix(mix, jsMixing.mixes)
              subscriber.push(election, getElectionTypeMixing(election))
            case Failure(err) =>
              println(s"Future error: ${getMessageFromThrowable(err)}")
          }
        case 2 =>
          val futureMix = subscriber.addMix[_1]()
          futureMix onComplete {
            case Success(mix) => 
              val election = addMix(mix, jsMixing.mixes)
              subscriber.push(election, getElectionTypeMixing(election))
            case Failure(err) =>
              println(s"Future error: ${getMessageFromThrowable(err)}")
          }
        case 3 =>
          val futureMix = subscriber.addMix[_2]()
          futureMix onComplete {
            case Success(mix) => 
              val election = addMix(mix, jsMixing.mixes)
              subscriber.push(election, getElectionTypeMixing(election))
            case Failure(err) =>
              println(s"Future error: ${getMessageFromThrowable(err)}")
          }
        case 4 =>
          val futureMix = subscriber.addMix[_3]()
          futureMix onComplete {
            case Success(mix) => 
              val election = addMix(mix, jsMixing.mixes)
              subscriber.push(election, getElectionTypeMixing(election))
            case Failure(err) =>
              println(s"Future error: ${getMessageFromThrowable(err)}")
          }
        case 5 =>
          val futureMix = subscriber.addMix[_4]()
          futureMix onComplete {
            case Success(mix) => 
              val election = addMix(mix, jsMixing.mixes)
              subscriber.push(election, getElectionTypeMixing(election))
            case Failure(err) =>
              println(s"Future error: ${getMessageFromThrowable(err)}")
          }
        case 6 =>
          val futureMix = subscriber.addMix[_5]()
          futureMix onComplete {
            case Success(mix) => 
              val election = addMix(mix, jsMixing.mixes)
              subscriber.push(election, getElectionTypeMixing(election))
            case Failure(err) =>
              println(s"Future error: ${getMessageFromThrowable(err)}")
          }
        case 7 =>
          val futureMix = subscriber.addMix[_6]()
          futureMix onComplete {
            case Success(mix) => 
              val election = addMix(mix, jsMixing.mixes)
              subscriber.push(election, getElectionTypeMixing(election))
            case Failure(err) =>
              println(s"Future error: ${getMessageFromThrowable(err)}")
          }
        case 8 =>
          val futureMix = subscriber.addMix[_7]()
          futureMix onComplete {
            case Success(mix) => 
              val election = addMix(mix, jsMixing.mixes)
              subscriber.push(election, getElectionTypeMixing(election))
            case Failure(err) =>
              println(s"Future error: ${getMessageFromThrowable(err)}")
          }
        case _ =>
          val futureMix = subscriber.addMix[_8]()
          futureMix onComplete {
            case Success(mix) => 
              val election = addMix(mix, jsMixing.mixes)
              subscriber.push(election, getElectionTypeMixing(election))
            case Failure(err) =>
              println(s"Future error: ${getMessageFromThrowable(err)}")
          }
      }
    }
  }
  
  def pushStartMixing() {
    println("GG ElectionStateMaintainer::pushStartMixing")
    val futureStopVotes = subscriber.stopVotes()
    futureStopVotes onComplete {
      case Success(stopped) => 
        val election = startMixing(stopped)
        subscriber.push(election, getElectionTypeMixing(election))
      case Failure(err) =>
        println(s"Future error: ${getMessageFromThrowable(err)}")
    }
  }
  
  def pushVotesStopped(jsVotesStopped: JsVotesStopped) {
    println("GG ElectionStateMaintainer::pushVotesStopped")
    if(jsVotesStopped.lastAddVoteIndex < 0) {
      println(s"ERROR on pushVotesStopped: lastAddVoteIndex is negative but it must be non-negative: ${jsVotesStopped.lastAddVoteIndex}")
    } else {
      Try {
        com.github.nscala_time.time.Imports.DateTime.parse(jsVotesStopped.date)
      } match {
        case Success(date) =>
          val futureVotes = subscriber.addVotes(jsVotesStopped.lastAddVoteIndex)
          futureVotes onComplete {
            case Success(votes) =>
              val election = stopVotes(votes, jsVotesStopped.lastAddVoteIndex, date)
              subscriber.push(election, getElectionTypeVotesStopped(election))
            case Failure(err) =>
              println(s"Future error: ${getMessageFromThrowable(err)}")
          }
        case Failure(err) =>
          println(s"Try Date ${jsVotesStopped.date.toString} parse error: ${getMessageFromThrowable(err)}")
      }
    }
  }
  
  def pushVotes(jsVotes: JsVotes) {
    println("GG ElectionStateMaintainer::pushVotes")
    if(0 == jsVotes.addVoteIndex) {
      val futureCombined = subscriber.combineShares()
      futureCombined onComplete {
        case Success(combined) => 
          val election = startVotes(combined)
          subscriber.push(election, getElectionTypeVotes(election))
        case Failure(err) =>
          println(s"Future error: ${getMessageFromThrowable(err)}")
      }
    } else if(0 < jsVotes.addVoteIndex) {
      val futureVotes = subscriber.addVotes(jsVotes.addVoteIndex - 1)
      futureVotes onComplete {
        case Success(votes) => 
          val election = addVotes(votes, jsVotes.votes)
          subscriber.push(election, getElectionTypeVotes(election))
        case Failure(err) =>
          println(s"Future error: ${getMessageFromThrowable(err)}")
      }
    } else if(0 > jsVotes.addVoteIndex) {
      println(s"ERROR on pushVotes: addVoteIndex is negative but it must be non-negative: ${jsVotes.addVoteIndex}")
    }
  }
  
  def pushCombined(jsCombined: JsCombined) {
    println("GG ElectionStateMaintainer::pushCombined")
    val futureShare = subscriber.addShare[W]()
    futureShare onComplete {
      case Success(share) => 
        val election = combineShares(share, jsCombined.publicKey)
        subscriber.push(election, getElectionTypeCombined(election))
        dto.setPublicKeys(election)
      case Failure(err) =>
        println(s"Future error: ${getMessageFromThrowable(err)}")
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
          println(s"Future error: ${getMessageFromThrowable(e)}")
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
              println(s"Future error: ${getMessageFromThrowable(e)}")
          }
        case 2 => 
          val futureShare = subscriber.addShare[_1]()
          futureShare onComplete { 
            case Success(share) =>
              val election = addShare(share, jsShares.shares._1, jsShares.shares._2)
              subscriber.push(election, getElectionTypeShares(election))
            case Failure(e) => 
              println(s"Future error: ${getMessageFromThrowable(e)}")
          }
        case 3 => 
          val futureShare = subscriber.addShare[_2]()
          futureShare onComplete { 
            case Success(share) =>
              val election = addShare(share, jsShares.shares._1, jsShares.shares._2)
              subscriber.push(election, getElectionTypeShares(election))
            case Failure(e) => 
              println(s"Future error: ${getMessageFromThrowable(e)}")
          }
        case 4 => 
          val futureShare = subscriber.addShare[_3]()
          futureShare onComplete { 
            case Success(share) =>
              val election = addShare(share, jsShares.shares._1, jsShares.shares._2)
              subscriber.push(election, getElectionTypeShares(election))
            case Failure(e) => 
              println(s"Future error: ${getMessageFromThrowable(e)}")
          }
        case 5 => 
          val futureShare = subscriber.addShare[_4]()
          futureShare onComplete { 
            case Success(share) =>
              val election = addShare(share, jsShares.shares._1, jsShares.shares._2)
              subscriber.push(election, getElectionTypeShares(election))
            case Failure(e) => 
              println(s"Future error: ${getMessageFromThrowable(e)}")
          }
        case 6 => 
          val futureShare = subscriber.addShare[_5]()
          futureShare onComplete { 
            case Success(share) =>
              val election = addShare(share, jsShares.shares._1, jsShares.shares._2)
              subscriber.push(election, getElectionTypeShares(election))
            case Failure(e) => 
              println(s"Future error: ${getMessageFromThrowable(e)}")
          }
        case 7 => 
          val futureShare = subscriber.addShare[_6]()
          futureShare onComplete { 
            case Success(share) =>
              val election = addShare(share, jsShares.shares._1, jsShares.shares._2)
              subscriber.push(election, getElectionTypeShares(election))
            case Failure(e) => 
              println(s"Future error: ${getMessageFromThrowable(e)}")
          }
        case 8 => 
          val futureShare = subscriber.addShare[_7]()
          futureShare onComplete { 
            case Success(share) =>
              val election = addShare(share, jsShares.shares._1, jsShares.shares._2)
              subscriber.push(election, getElectionTypeShares(election))
            case Failure(e) => 
              println(s"Future error: ${getMessageFromThrowable(e)}")
          }
        case _ => 
          val futureShare = subscriber.addShare[_8]()
          futureShare onComplete { 
            case Success(share) =>
              val election = addShare(share, jsShares.shares._1, jsShares.shares._2)
              subscriber.push(election, getElectionTypeShares(election))
            case Failure(e) => 
              println(s"Future error: ${getMessageFromThrowable(e)}")
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
        new Election[W, Created](Created(jsElection.state.id, cSettings, uid, jsElection.dto))
      dto.setDTO(jsElection.dto)
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
            val jsElect = jSeqPost.get
            pushCreate(jSeqPost.get, jsElect.state.id)
            dto.setState(ElectionDTOData.REGISTERED)
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
                    val combined = b.get
                    pushCombined(combined)
                  case e: JsError =>
                    println(s"JsError error: ${e} message ${post.message}")
                }
              case "Votes" =>
                jsMessage.message.validate[JsVotes] match {
                  case b: JsSuccess[JsVotes] =>
                    pushVotes(b.get)
                    dto.setState(ElectionDTOData.STARTED)
                  case e: JsError =>
                    println(s"JsError error: ${e} message ${post.message}")
                }
              case "VotesStopped" =>
                jsMessage.message.validate[JsVotesStopped] match {
                  case b: JsSuccess[JsVotesStopped] =>
                    pushVotesStopped(b.get)
                    dto.setState(ElectionDTOData.STOPPED)
                  case e: JsError =>
                    println(s"JsError error: ${e} message ${post.message}")
                }
              case "StartMixing" =>
                if(JsNull == jsMessage.message) {
                  pushStartMixing()
                    dto.setState(ElectionDTOData.DOING_TALLY)
                } else {
                  println(s"Error: StartMixing : message is not null: message ${post.message}")
                }
              case "Mixing" =>
                jsMessage.message.validate[JsMixing] match {
                  case b: JsSuccess[JsMixing] =>
                    pushMixing(b.get)
                  case e: JsError =>
                    println(s"JsError error: ${e} message ${post.message}")
                }
              case "Mixed" =>
                if(JsNull == jsMessage.message) {
                  pushStopMixing()
                } else {
                  println(s"Error: StopMixing : message is not null: message ${post.message}")
                }
              case "StartDecryptions" =>
                if(JsNull == jsMessage.message) {
                  pushStartDecryptions()
                } else {
                  println(s"Error: StartDecryptions : message is not null: message ${post.message}")
                }
              case "Decryptions" =>
                jsMessage.message.validate[JsDecryptions] match {
                  case b: JsSuccess[JsDecryptions] =>
                    pushDecryptions(b.get)
                  case e: JsError =>
                    println(s"JsError error: ${e} message ${post.message}")
                }
              case "Decrypted" =>
                jsMessage.message.validate[JsDecrypted] match {
                  case b: JsSuccess[JsDecrypted] =>
                    pushDecrypted(b.get)
                    dto.setState(ElectionDTOData.RESULTS_OK)
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