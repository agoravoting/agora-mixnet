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
import ops.nat._

import app._
import scala.concurrent.{Future, Promise}
import scala.util.{Try, Success, Failure}
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import election._

class ElectionSubscriber[W <: Nat : ToInt](val uid : String) extends GetType {
  implicit val system = ActorSystem()
  implicit val executor = system.dispatchers.lookup("my-other-dispatcher")
  implicit val materializer = ActorMaterializer()
  println("GG ElectionSubscriber::constructor")
  private val map = scala.collection.mutable.Map[String, Any]()
  
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
    println("GG ElectionSubscriber::push electionType " + electionType + " uid " + election.state.uid)
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
    println("GG ElectionSubscriber::pull electionType " + electionType)
    val realPromise = Promise[Election[W, B]]()
    Future {
      val promise = Promise[Election[W, B]]()
      val any = getOrAdd(electionType, promise)
      Try {
        any.asInstanceOf[Promise[Election[W, B]]]
      } match {
        case Success(p) =>
          realPromise.completeWith(p.future)
        case Failure(e) =>
          realPromise.failure(e)
      }
    }
    realPromise.future
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
  
  def addMix[T <: Nat : ToInt](): Future[Election[W, Mixing[T]]]  = 
    pull[Mixing[T]](getElectionMixing[W, T])
  
  def stopMixing() : Future[Election[W, Mixed]]  = 
    pull[Mixed](getElectionMixed[W])
  
  def startDecryptions() : Future[Election[W, Decryptions[_0]]]  = 
    pull[Decryptions[_0]](getElectionDecryptions[W, _0])
  
  def addDecryption[T <: Nat : ToInt](): Future[Election[W, Decryptions[T]]]  = 
    pull[ Decryptions[T]](getElectionDecryptions[W, T])
  
  def combineDecryptions() : Future[Election[W, Decrypted]]  = 
    pull[Decrypted](getElectionDecrypted[W])
}