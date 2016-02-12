package mpservice

import akka.actor.{ Actor, Props, UnboundedStash, ActorLogging, Inbox, ActorRef }
import scala.concurrent.duration.DurationInt
import akka.routing.FromConfig
import akka.actor.ActorSystem
import java.math.BigInteger
import scala.collection._
import com.typesafe.config.ConfigFactory
import scala.util.Try
import scala.util.Success

// public api
case class ModPow(base: BigInteger, pow: BigInteger, mod: BigInteger)
trait ModPowService {
  def compute(work: Array[ModPow]): Array[BigInteger]
}

// implementation
case class Work(requestId: Int, workId: Int, work: Array[ModPow])
case class WorkReply(requestId: Int, workId: Int, result: Array[BigInteger])
case class ModPowArray(modpows: Array[ModPow])
case class ModPowArrayResult(result: Array[BigInteger])
case class RequestData(client: ActorRef, length: Int, results: mutable.ArrayBuffer[WorkReply])

class AkkaModPowService(system: ActorSystem, modPowService: ActorRef) extends ModPowService {
  val inbox = Inbox.create(system)

  def compute(work: Array[ModPow]): Array[BigInteger] = {
    inbox.send(modPowService, ModPowArray(work))
    Try(inbox.receive(1000.seconds)) match {
      case Success(ModPowArrayResult(answer)) => answer
      case _ => println("Fuck up"); Array(new BigInteger("23"))
    }
  }
}

class ModPowServiceActor(val sendIntervalMillis: Int, val dataArraySize: Int) extends Actor with ActorLogging {
  
  val workerRouter = context.actorOf(WorkerActor.props(0).withRouter(FromConfig()), name = "workerRouter")

  // actor state
  var cores = 8
  var requestId = 0
  val requests = mutable.Map[Int, RequestData]()

  def receive: Receive = {
    
    case ModPowArray(modpows) => {
      requestId = requestId + 1
      val size = math.min(modpows.length / cores, 3000)
      val chunks = modpows.sliding(size, size).toArray
      requests.put(requestId, RequestData(sender, chunks.length, mutable.ArrayBuffer()))
      println(s"request with ${modpows.length} units, splitting into ${chunks.length} chunks")
      // YUCK FIXME
      var counter = 0
      chunks.indices.foreach { i =>
        val work = Work(requestId, i, chunks(i))
        println(s"Sending chunk $work")
        workerRouter ! work
      }
    }
    
    case w: WorkReply => {
      val requestData = requests.get(w.requestId).get
      requestData.results += w
      if(requestData.results.length == requestData.length) {
        requests.remove(w.requestId)
        val sorted = requestData.results.sortWith(_.workId < _.workId)
        requestData.client ! ModPowArrayResult(sorted.flatMap(_.result).toArray)
      }
    }

  }
}

class WorkerActor(val processingTimeMillis: Int) extends Actor with ActorLogging {
  def receive: Receive = {
    case Work(requestId, workId, modpows) => {
      println(s"received request length ${modpows.length} at actor $this")
      val result = modpows.map(x => x.base.modPow(x.pow, x.mod))
      sender ! WorkReply(requestId, workId, result)
    }
  }
}

object WorkerActor {
  def props(processingTimeMillis: Int): Props = Props(new WorkerActor(processingTimeMillis))
}

object ModPowServiceActor {
  def props(sendIntervalMillis: Int, dataArraySize: Int): Props = Props(new ModPowServiceActor(sendIntervalMillis, dataArraySize))
}

object WorkerApp {

  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.load()
    val system = ActorSystem("ClusterSystem", config)
  }
}

object TestApp {

  def main(args: Array[String]) {
    val config = ConfigFactory.load()
    val system = ActorSystem("ClusterSystem", config)

    // val metricsIntervalSeconds = config.getInt("producer.metrics-interval-seconds")
    // system.actorOf(ClusterListener.props(metricsIntervalSeconds))
    val serviceActor = system.actorOf(ModPowServiceActor.props(2, 2), name = "ModPowService")
    val service = new AkkaModPowService(system, serviceActor)

    // while(true) {
      Thread sleep 5000
      val input = Array.fill(100000)(rndModPow)

      println("akka service")
      var now = System.currentTimeMillis
      val answerOne = service.compute(input)
      var elapsed = (System.currentTimeMillis - now) / 1000.0
      println(s"elapsed $elapsed")

      println("serial..")
      now = System.currentTimeMillis
      val answerTwo = input.map(m => m.base.modPow(m.pow, m.mod))
      elapsed = (System.currentTimeMillis - now) / 1000.0
      println(s"elapsed $elapsed")      


      println(answerOne.deep == answerTwo.deep)
      system.shutdown() 
    // }
  }

  def rndModPow() = {
    ModPow(rndBigInt.underlying, rndBigInt.underlying, rndBigInt.underlying)
  }

  def rndBigInt = {
    BigInt(1024, new scala.util.Random)  
  }
}