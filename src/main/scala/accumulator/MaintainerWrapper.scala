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

import models._

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
  
  def getElectionInfo() = {
    maintainer.getElectionInfo()
  }
  
  def getResults() : Option[String] = {
    maintainer.getResults()
  }
}
