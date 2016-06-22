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
import ops.nat._

import app._
import election._

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
    s"app.Election[shapeless.nat._${toInt[W]},app.Votes[${election.state.addVoteIndex}]]"
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