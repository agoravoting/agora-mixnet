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
import com.squareup.jnagmp.Gmp

/******************** PUBLIC API ********************/

/**
 * Represents a modular exponentiation operation
 */
case class ModPow(base: BigInteger, pow: BigInteger, mod: BigInteger)

/**
 * Represents a modular exponentiation operation with common modulus (see below)
 */
case class ModPow2(base: BigInteger, pow: BigInteger)

/**
 * Represents a modular exponentiation operation with common modulus and result
 */
case class ModPowResult(base: BigInteger, pow: BigInteger, mod: BigInteger, result: BigInteger)

/**
 * The mpservice public api
 */
trait ModPowService {
  // compute modular exponentiation for a list of inputs
  def compute(work: Array[ModPow]): Array[BigInteger]
  // compute modular exponentiation for a list of inputs with common modulus
  def compute(work: Array[ModPow2], mod: BigInteger): Array[BigInteger]

  def computeDebug(work: Array[ModPow2], mod: BigInteger): Array[ModPowResult]
}

/******************** IMPLEMENTATION ********************/

object MPService extends ModPowService {
  // FIXME should be configurable
  val service = AkkaModPowService

  def compute(work: Array[ModPow]): Array[BigInteger] = service.compute(work)
  def compute(work: Array[ModPow2], mod: BigInteger): Array[BigInteger] = service.compute(work, mod)
  def computeDebug(work: Array[ModPow2], mod: BigInteger): Array[ModPowResult] = service.computeDebug(work, mod)

  def shutdown = service.shutdown
  def init = {}
  override def toString = service.getClass.toString
}

object MPBridgeS {
  // FIXME move to Util
  val generatorParallelism = ConfigFactory.load().getInt("generators-parallelism-level")

  def ex[T](f: => T, v: String) = {
    MPBridge.a()
    MPBridge.startRecord(v)
    val now = System.currentTimeMillis
    var ret = f
    val r = System.currentTimeMillis - now
    println(s"R: [$r ms]")
    val requests = MPBridge.stopRecord()
    MPBridge.b(3)
    if(requests.length > 0) {
        val now2 = System.currentTimeMillis
        val answers = MPService.compute(requests, MPBridge.getModulus);
        val c = System.currentTimeMillis - now2
        MPBridge.startReplay(answers)
        ret = f
        val t = System.currentTimeMillis - now
        println(s"\nC: [$c ms] T: [$t ms] R+C: [${r+c} ms]")
        MPBridge.stopReplay()
    }
    MPBridge.reset()

    ret
  }

  def init(useGmp: Boolean, useExtractor: Boolean) = MPBridge.init(useGmp, useExtractor)
  def shutdown = MPBridge.shutdown

  import ch.bfh.unicrypt.math.algebra.general.abstracts.AbstractCyclicGroup
  import ch.bfh.unicrypt.helper.random.deterministic.DeterministicRandomByteSequence
  import ch.bfh.unicrypt.helper.random.deterministic.CTR_DRBG
  import scala.collection.JavaConversions._
  import ch.bfh.unicrypt.helper.converter.classes.biginteger.ByteArrayToBigInteger
  import ch.bfh.unicrypt.helper.math.MathUtil
  import ch.bfh.unicrypt.helper.array.classes.DenseArray
  import ch.bfh.unicrypt.helper.sequence.Sequence
  import ch.bfh.unicrypt.math.algebra.general.interfaces.Element
  import scala.collection.JavaConversions._

  // FIXME move to Util
  def getIndependentGenerators[E <: Element[_]](group: AbstractCyclicGroup[E, _], skip: Int, size: Int): java.util.List[E] = {

    val split = generatorParallelism
    val total = size + skip

    val a = Array.fill(total % split)((total / split) + 1)
    val b = Array.fill(split - (total % split))(total / split)
    val c = a ++ b

    val seedLength = CTR_DRBG.getFactory().getSeedByteLength()
    val converter = ByteArrayToBigInteger.getInstance(seedLength)

    val rds = c.zipWithIndex.map{ case (value, index) =>
      // 1000: we want to leave room for generators not to overlap
      val seed = java.math.BigInteger.valueOf(index * (total / split) * 1000).mod(MathUtil.powerOfTwo(CTR_DRBG.getFactory().getSeedByteLength()))
      // println("*** index " + index + " seed " + seed + " value " + value)
      val r = DeterministicRandomByteSequence.getInstance(CTR_DRBG.getFactory(), converter.reconvert(seed))
      (r, value)
    }
    // rds.foreach(println)

    val items = rds.par.flatMap { case (d, i) =>
      val sequence = group.getIndependentGenerators(d).limit(i)
      sequence.toList
    }
    println("getIndependentGenerators " + total + " " + items.size)

    // DenseArray.getInstance(items.drop(skip).toList.toArray)
    items.drop(skip).toList
  }
}

object SequentialModPowService extends ModPowService {
  def compute(work: Array[ModPow]): Array[BigInteger] = work.map(x => x.base.modPow(x.pow, x.mod))
  def compute(work: Array[ModPow2], mod: BigInteger): Array[BigInteger] = work.map(x => x.base.modPow(x.pow, mod))
  def computeDebug(work: Array[ModPow2], mod: BigInteger): Array[ModPowResult] = {
    work.map(x => ModPowResult(x.base, x.pow, mod, x.base.modPow(x.pow, mod))).seq.toArray
  }
}
object GmpParallelModPowService extends ModPowService {
  def compute(work: Array[ModPow]): Array[BigInteger] = {
    work.par.map(x => Gmp.modPowInsecure(x.base, x.pow, x.mod)).seq.toArray
  }
  def compute(work: Array[ModPow2], mod: BigInteger): Array[BigInteger] = {
    work.par.map(x => Gmp.modPowInsecure(x.base, x.pow, mod)).seq.toArray
  }
  def computeDebug(work: Array[ModPow2], mod: BigInteger): Array[ModPowResult] = {
    work.par.map(x => ModPowResult(x.base, x.pow, mod, Gmp.modPowInsecure(x.base, x.pow, mod))).seq.toArray
  }
}
object ParallelModPowService extends ModPowService {
  def compute(work: Array[ModPow]): Array[BigInteger] = work.par.map(x => x.base.modPow(x.pow, x.mod)).seq.toArray
  def compute(work: Array[ModPow2], mod: BigInteger): Array[BigInteger] = work.par.map(x => x.base.modPow(x.pow, mod)).seq.toArray
  def computeDebug(work: Array[ModPow2], mod: BigInteger): Array[ModPowResult] = {
    work.par.map(x => ModPowResult(x.base, x.pow, mod, x.base.modPow(x.pow, mod))).seq.toArray
  }
}

/******************** AKKA ********************/

// messaging classes
case class Work(requestId: Int, workId: Int, work: Array[ModPow])
case class WorkFixedMod(requestId: Int, workId: Int, work: Array[ModPow2], mod: BigInteger)
case class WorkReply(requestId: Int, workId: Int, result: Array[BigInteger])
case class ModPowArray(modpows: Array[ModPow])
case class ModPowArrayFixedMod(modpows: Array[ModPow2], mod: BigInteger)
case class ModPowArrayResult(result: Array[BigInteger])

// debugging
case class WorkFixedModDebug(requestId: Int, workId: Int, work: Array[ModPow2], mod: BigInteger)
case class WorkReplyDebug(requestId: Int, workId: Int, result: Array[ModPowResult])
case class ModPowArrayFixedModDebug(modpows: Array[ModPow2], mod: BigInteger)
case class ModPowArrayResultDebug(result: Array[ModPowResult])

class AkkaModPowService(system: ActorSystem, modPowService: ActorRef) extends ModPowService {

  def compute(work: Array[ModPow]): Array[BigInteger] = {
    val inbox = Inbox.create(system)

    inbox.send(modPowService, ModPowArray(work))
    Try(inbox.receive(1000.seconds)) match {
      case Success(ModPowArrayResult(answer)) => answer
      // FIXME
      case _ => throw new Exception()
    }
  }
  def compute(work: Array[ModPow2], mod: BigInteger): Array[BigInteger] = {
    val inbox = Inbox.create(system)

    inbox.send(modPowService, ModPowArrayFixedMod(work, mod))
    Try(inbox.receive(1000.seconds)) match {
      case Success(ModPowArrayResult(answer)) => answer
      // FIXME
      case _ => throw new Exception()
    }
  }
  def computeDebug(work: Array[ModPow2], mod: BigInteger): Array[ModPowResult] = {
    val inbox = Inbox.create(system)

    inbox.send(modPowService, ModPowArrayFixedModDebug(work, mod))
    Try(inbox.receive(1000.seconds)) match {
      case Success(ModPowArrayResultDebug(answer)) => answer
      // FIXME
      case _ => throw new Exception()
    }
  }
}

object AkkaModPowService extends ModPowService {
  val config = ConfigFactory.load()
  val system = ActorSystem("ClusterSystem", config)
  val maxChunkSize = config.getInt("master.max-chunk-size")
  val sendDelay = config.getInt("master.send-delay-ms")
  val minChunks = config.getInt("master.min-chunk")
  val useGmp = config.getBoolean("mpservice.use-gmp")

  val serviceActor = system.actorOf(ModPowServiceActor.props(minChunks, maxChunkSize, sendDelay, useGmp), name = "ModPowService")
  val service = new AkkaModPowService(system, serviceActor)

  def compute(work: Array[ModPow]): Array[BigInteger] = service.compute(work)
  def compute(work: Array[ModPow2], mod: BigInteger): Array[BigInteger] = service.compute(work, mod)
  def computeDebug(work: Array[ModPow2], mod: BigInteger): Array[ModPowResult] = service.computeDebug(work, mod)
  def shutdown = system.terminate
}

class ModPowServiceActor(val minChunks: Int, val maxChunkSize: Int, val sendDelay: Int, val useGmp: Boolean) extends Actor with ActorLogging {
  case class RequestData(client: ActorRef, length: Int, results: mutable.ArrayBuffer[WorkReply], sent: Long = System.currentTimeMillis)
  case class RequestDataDebug(client: ActorRef, length: Int, results: mutable.ArrayBuffer[WorkReplyDebug], sent: Long = System.currentTimeMillis)

  val workerRouter = context.actorOf(WorkerActor.props(useGmp).withRouter(FromConfig()), name = "workerRouter")

  // actor state
  var requestId = 0
  val requests = mutable.Map[Int, RequestData]()
  val requestsDebug = mutable.Map[Int, RequestDataDebug]()


  // FIXME move to util
  def cut[A](xs: Array[A], n: Int) = {
    val (quot, rem) = (xs.size / n, xs.size % n)
    val (smaller, bigger) = xs.splitAt(xs.size - rem * (quot + 1))
    smaller.grouped(quot) ++ bigger.grouped(quot + 1)
  }

  def receive: Receive = {

    case ModPowArray(modpows) => {
      requestId = requestId + 1
      val size = math.min(math.max(modpows.length / minChunks, 1), maxChunkSize)
      val chunks: Array[Array[ModPow]] = cut(modpows, modpows.length / size).toArray
      requests.put(requestId, RequestData(sender, chunks.length, mutable.ArrayBuffer()))
      println(s"request with ${modpows.length} units, splitting into ${chunks.length} chunks")
      chunks.indices.foreach { i =>
        val work = Work(requestId, i, chunks(i))

        Thread sleep sendDelay
        workerRouter ! work
      }
    }

    case ModPowArrayFixedMod(modpows, mod) => {
      requestId = requestId + 1
      val size = math.min(math.max(modpows.length / minChunks, 1), maxChunkSize)
      val chunks: Array[Array[ModPow2]] = cut(modpows, modpows.length / size).toArray
      requests.put(requestId, RequestData(sender, chunks.length, mutable.ArrayBuffer()))
      println(s"request with ${modpows.length} units, splitting into ${chunks.length} chunks")
      chunks.indices.foreach { i =>
        val work = WorkFixedMod(requestId, i, chunks(i), mod)

        Thread sleep sendDelay
        workerRouter ! work
      }
    }

    case ModPowArrayFixedModDebug(modpows, mod) => {
      requestId = requestId + 1
      val size = math.min(math.max(modpows.length / minChunks, 1), maxChunkSize)
      val chunks: Array[Array[ModPow2]] = cut(modpows, modpows.length / size).toArray
      requestsDebug.put(requestId, RequestDataDebug(sender, chunks.length, mutable.ArrayBuffer()))
      println(s"DBG request with ${modpows.length} units, splitting into ${chunks.length} chunks")
      chunks.indices.foreach { i =>
        val work = WorkFixedModDebug(requestId, i, chunks(i), mod)

        Thread sleep sendDelay
        workerRouter ! work
      }
    }

    case w: WorkReply => {
      val requestData = requests.get(w.requestId).get
      requestData.results += w
      val diff = System.currentTimeMillis - requestData.sent
      // println(s"${w.requestId} ${w.workId} $diff")
      if(requestData.results.length == requestData.length) {
        requests.remove(w.requestId)
        val sorted = requestData.results.sortWith(_.workId < _.workId)
        requestData.client ! ModPowArrayResult(sorted.flatMap(_.result).toArray)
      }
    }

    case w: WorkReplyDebug => {
      val requestData = requestsDebug.get(w.requestId).get
      requestData.results += w
      val diff = System.currentTimeMillis - requestData.sent
      // println(s"${w.requestId} ${w.workId} $diff")
      if(requestData.results.length == requestData.length) {
        requestsDebug.remove(w.requestId)
        val sorted = requestData.results.sortWith(_.workId < _.workId)
        requestData.client ! ModPowArrayResultDebug(sorted.flatMap(_.result).toArray)
      }
    }
  }
}

class WorkerActor(val useGmp: Boolean) extends Actor with ActorLogging {
  val service = if(useGmp) GmpParallelModPowService else ParallelModPowService

  def receive: Receive = {
    case Work(requestId, workId, modpows) => {
      // println(s"received request length ${modpows.length} at actor $this")
      val before = System.currentTimeMillis
      val result = service.compute(modpows).seq.toArray
      val diff = (System.currentTimeMillis - before)
      print("+")
      // println(s"$requestId $workId $diff")
      sender ! WorkReply(requestId, workId, result)
    }
    case WorkFixedMod(requestId, workId, modpows, mod) => {
      // println(s"received request length ${modpows.length} at actor $this")
      val before = System.currentTimeMillis
      val result = service.compute(modpows, mod).seq.toArray
      val diff = (System.currentTimeMillis - before)
      print("=")
      // println(s"$requestId $workId $diff")
      sender ! WorkReply(requestId, workId, result)
    }
    case WorkFixedModDebug(requestId, workId, modpows, mod) => {
      // println(s"received request length ${modpows.length} at actor $this")
      val before = System.currentTimeMillis
      val result = service.computeDebug(modpows, mod).seq.toArray
      val diff = (System.currentTimeMillis - before)
      print("=")
      // println(s"$requestId $workId $diff")
      sender ! WorkReplyDebug(requestId, workId, result)
    }
  }
}

object WorkerActor {
  def props(useGmp: Boolean): Props = Props(new WorkerActor(useGmp))
}

object ModPowServiceActor {
  def props(minChunks: Int, maxChunkSize: Int, sendDelay: Int, useGmp: Boolean): Props = Props(new ModPowServiceActor(minChunks, maxChunkSize, sendDelay, useGmp))
}

/******************** LAUNCHER ********************/

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

    val maxChunkSize = config.getInt("master.max-chunk-size")
    val sendDelay = config.getInt("master.send-delay-ms")
    val minChunks = config.getInt("master.min-chunk")
    val useGmp = config.getBoolean("mpservice.use-gmp")
    // val metricsIntervalSeconds = config.getInt("producer.metrics-interval-seconds")
    // system.actorOf(ClusterListener.props(metricsIntervalSeconds))
    val serviceActor = system.actorOf(ModPowServiceActor.props(minChunks, maxChunkSize, sendDelay, useGmp), name = "ModPowService")
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
      system.terminate
    // }
  }

  def rndModPow() = {
    ModPow(rndBigInt.underlying, rndBigInt.underlying, rndBigInt.underlying)
  }

  def rndBigInt = {
    BigInt(1024, new scala.util.Random)
  }
}