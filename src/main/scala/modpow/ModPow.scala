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
      // FIXME
      case _ => throw new Exception()
    }
  }
}

class DummyModPowService extends ModPowService {
  def compute(work: Array[ModPow]): Array[BigInteger] = work.map(x => x.base.modPow(x.pow, x.mod))
}


object AkkaModPowService extends ModPowService {
  val config = ConfigFactory.load()
  val system = ActorSystem("ClusterSystem", config)
  val serviceActor = system.actorOf(ModPowServiceActor.props(200, 0), name = "ModPowService")
  val service = new AkkaModPowService(system, serviceActor)

  def compute(work: Array[ModPow]): Array[BigInteger] = service.compute(work)
  def shutdown = system.shutdown
}

object MPService extends ModPowService {
  val service = AkkaModPowService

  def compute(work: Array[ModPow]): Array[BigInteger] = service.compute(work)
  def shutdown = service.shutdown
}

class ModPowServiceActor(val maxChunkSize: Int, val sendDelay: Int) extends Actor with ActorLogging {
  
  val workerRouter = context.actorOf(WorkerActor.props(0).withRouter(FromConfig()), name = "workerRouter")
  val minimumParallelism = 2

  // actor state
  var requestId = 0
  val requests = mutable.Map[Int, RequestData]()

  def receive: Receive = {
    
    case ModPowArray(modpows) => {
      requestId = requestId + 1
      val size = math.min(math.max(modpows.length / minimumParallelism, 1), maxChunkSize)
      val chunks = modpows.sliding(size, size).toArray
      requests.put(requestId, RequestData(sender, chunks.length, mutable.ArrayBuffer()))
      println(s"request with ${modpows.length} units, splitting into ${chunks.length} chunks")
      chunks.indices.foreach { i =>
        val work = Work(requestId, i, chunks(i))
        // println(s"Sending chunk $work")
        
        Thread sleep sendDelay
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
      // println(s"received request length ${modpows.length} at actor $this")
      val result = modpows.par.map(x => x.base.modPow(x.pow, x.mod)).seq.toArray
      print("+")
      sender ! WorkReply(requestId, workId, result)
    }
  }
}

object WorkerActor {
  def props(processingTimeMillis: Int): Props = Props(new WorkerActor(processingTimeMillis))
}

object ModPowServiceActor {
  def props(maxChunkSize: Int, sendDelay: Int): Props = Props(new ModPowServiceActor(maxChunkSize, sendDelay))
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
    val serviceActor = system.actorOf(ModPowServiceActor.props(300, 100), name = "ModPowService")
    val service = new AkkaModPowService(system, serviceActor)

    val total = 300000
    // while(true) {
      println("Hit return to start")
      Console.in.read()
      val input = Array.fill(total)(rndModPow)

      println("akka service")
      var now = System.currentTimeMillis
      val answerOne = service.compute(input)
      var elapsed = (System.currentTimeMillis - now) / 1000.0
      println(s"elapsed ${total / elapsed}")

      println("serial..")
      now = System.currentTimeMillis
      val answerTwo = input.map(m => m.base.modPow(m.pow, m.mod))
      elapsed = (System.currentTimeMillis - now) / 1000.0
      println(s"elapsed ${total / elapsed}")      


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

object MPE {
  def ex[T](f: => T, v: String) = {
    ch.MP.a()
    ch.MP.startRecord(v)
    var ret = f
    val requests = ch.MP.stopRecord()
    ch.MP.b(3)
    if(requests.length > 0) {
        val answers = mpservice.MPService.compute(requests);
        ch.MP.startReplay(answers)
        ret = f
        ch.MP.stopReplay()
    }
    ch.MP.reset()

    ret
  }
}