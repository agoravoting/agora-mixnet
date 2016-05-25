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
}
