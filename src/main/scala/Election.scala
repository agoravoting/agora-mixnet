import shapeless._
import nat._
import syntax.sized._
import ops.nat._
import LT._
import com.github.nscala_time.time.Imports._
import com.typesafe.config.ConfigFactory

import ch.bfh.unicrypt.math.algebra.multiplicative.classes.GStarModSafePrime
import ch.bfh.unicrypt.crypto.schemes.encryption.classes.ElGamalEncryptionScheme
import ch.bfh.unicrypt.math.algebra.general.interfaces.Element
import ch.bfh.unicrypt.math.algebra.general.classes.Pair
import ch.bfh.unicrypt.math.algebra.general.classes.Tuple
import ch.bfh.unicrypt.crypto.encoder.classes.ZModPrimeToGStarModSafePrime
import ch.bfh.unicrypt.crypto.encoder.interfaces.Encoder
import ch.bfh.unicrypt.math.algebra.general.abstracts.AbstractSet
import ch.bfh.unicrypt.math.algebra.general.classes.ProductSet
import mpservice.MPBridgeS
import mpservice.MPBridge

import scala.collection.JavaConversions._


/**
 * An election is a typed, purely function state machine with an immutable history
 *
 * The parameters are privacy level, W, and current state, S
 *
 */
class Election[+W <: Nat, +S <: ElectionState] private (val state: S) {
  override def toString() = s"election ${state.id}, ${state.toString}"
}

/**
 * These types represent the state of the election and associated information
 *
 */
case class Created(override val id: String, override val cSettings: CryptoSettings) extends ElectionState(id, cSettings)
case class Shares[T <: Nat](val shares: Sized[List[(String, String)], T], prev: ElectionState) extends ElectionStateShares(prev.id, prev.cSettings, shares.toList) with HasHistory
case class Combined(override val publicKey: String, prev: ElectionStateShares) extends ElectionStatePk(prev.id, prev.cSettings, prev.allShares, publicKey) with HasHistory
case class Votes(votes: List[String], prev: ElectionStatePk) extends ElectionStatePk(prev.id, prev.cSettings, prev.allShares, prev.publicKey) with HasHistory
case class VotesStopped(prev: Votes, date: DateTime = DateTime.now) extends ElectionStateVotes(prev.id, prev.cSettings, prev.allShares, prev.publicKey, prev.votes) with HasHistory
case class Mixing[T <: Nat](mixes: Sized[List[ShuffleResultDTO], T], prev: ElectionStateVotes) extends ElectionStateVotes(prev.id, prev.cSettings, prev.allShares, prev.publicKey, prev.votes) with HasHistory
case class Mixed(prev: Mixing[_ <: Nat]) extends ElectionStateVotes(prev.id, prev.cSettings, prev.allShares, prev.publicKey, prev.votes) with HasHistory
case class Decryptions[T <: Nat](decryptions: Sized[List[PartialDecryptionDTO], T], prev: ElectionStateVotes) extends ElectionStateVotes(prev.id, prev.cSettings, prev.allShares, prev.publicKey, prev.votes) with HasHistory
case class Decrypted(decrypted: Seq[String], prev: Decryptions[_ <: Nat]) extends ElectionStateVotes(prev.id, prev.cSettings, prev.allShares, prev.publicKey, prev.votes) with HasHistory

/**
 * The state machine transitions
 *
 * Method signatures allow the compiler to enforce the state machine logic.
 */
object Election {

  // create an election
  def create[W <: Nat](id: String, bits: Int) = {
    println("Going to start a new Election!")
    val group = GStarModSafePrime.getFirstInstance(bits)
    val generator = group.getDefaultGenerator()
    val cSettings = CryptoSettings(group, generator)

    new Election[W, Created](Created(id, cSettings))
  }

  // now ready to receive shares
  def startShares[W <: Nat](in: Election[W, Created]) = {
    println("Now waiting for shares")
    new Election[W, Shares[_0]](Shares[_0](List[(String, String)]().sized(0).get, in.state))
  }

  // verify and add a share
  def addShare[W <: Nat, T <: Nat](in: Election[W, Shares[T]], share: EncryptionKeyShareDTO, proverId: String)(implicit ev: T < W) = {
    println(s"Adding share...")

    val result = Verifier.verifyKeyShare(share, in.state.cSettings, proverId)
    if(result) {
      new Election[W, Shares[Succ[T]]](Shares[Succ[T]](in.state.shares :+ (proverId, share.keyShare), in.state))
    }
    else {
      throw new Exception("Share failed verification")
    }
  }

  // combine the shares into a public key, can only happen if we have all the shares
  def combineShares[W <: Nat](in: Election[W, Shares[W]]) = {
    println("Combining shares..")

    val shares = in.state.shares.map { s =>
      Util.getPublicKeyFromString(s._2, in.state.cSettings.generator)
    }
    val publicKey = shares.reduce( (a,b) => a.apply(b) )

    new Election[W, Combined](Combined(publicKey.convertToString, in.state))
  }

  // start the voting period
  def startVotes[W <: Nat](in: Election[W, Combined]) = {
    println("Now waiting for votes")
    new Election[W, Votes](Votes(List[String](), in.state))
  }

  // votes are cast here
  def addVotes[W <: Nat](in: Election[W, Votes], vote: String) = {
    print("+")
    
    // removed for testing faster
    /*
    val elGamal = ElGamalEncryptionScheme.getInstance(in.state.cSettings.generator)
    // this will throw exception if the vote is invalid
    elGamal.getEncryptionSpace.getElementFromString(vote)
    */

    new Election[W, Votes](Votes(vote :: in.state.votes, in.state))
  }

  // stop election period
  def stopVotes[W <: Nat](in: Election[W, Votes]) = {
    println("No more votes")
    new Election[W, VotesStopped](VotesStopped(in.state))
  }

  // start mixing
  def startMixing[W <: Nat](in: Election[W, VotesStopped]) = {
    println("Now waiting for mixes")
    new Election[W, Mixing[_0]](Mixing[_0](List[ShuffleResultDTO]().sized(0).get, in.state))
  }

  // add a mix by a mixer trustee
  def addMix[W <: Nat, T <: Nat](in: Election[W, Mixing[T]], mix: ShuffleResultDTO, proverId: String)(implicit ev: T < W) = {
    println("Adding mix...")
    val elGamal = ElGamalEncryptionScheme.getInstance(in.state.cSettings.generator)
    val keyPairGen = elGamal.getKeyPairGenerator()
    val publicKey = keyPairGen.getPublicKeySpace().getElementFrom(in.state.publicKey)
    
    println("Convert votes...")
    
    val now = System.currentTimeMillis
    
    // will be slightly faster but will not scale over the cluster
    /*
    MPBridge.a()
    val shuffled = mix.votes.par.map( v => elGamal.getEncryptionSpace.getElementFromString(v, true) ).seq
    val votes = in.state match {
      case s: Mixing[_0] => in.state.votes.par.map( v => elGamal.getEncryptionSpace.getElementFromString(v, true) ).seq
      case _ => in.state.mixes.toList.last.votes.par.map( v => elGamal.getEncryptionSpace.getElementFromString(v, true) ).seq
    }
    MPBridge.b()
    */

    val shuffled = mix.votes.par.map( v => Util.getE(elGamal.getEncryptionSpace, v) ).seq
    val votes = in.state match {
      case s: Mixing[_0] => in.state.votes.par.map( v => Util.getE(elGamal.getEncryptionSpace, v) ).seq
      case _ => in.state.mixes.toList.last.votes.par.map( v => Util.getE(elGamal.getEncryptionSpace, v) ).seq
    }
    println(s"* vote conversion: ${System.currentTimeMillis - now}")
    
    /*val (shuffled, votes) = MPBridgeS.ex({
      val shuffled = mix.votes.map( v => elGamal.getEncryptionSpace.getElementFromString(v) ).seq
      val votes = in.state match {
        case s: Mixing[_0] => in.state.votes.map( v => elGamal.getEncryptionSpace.getElementFromString(v) ).seq
        case _ => in.state.mixes.toList.last.votes.map( v => elGamal.getEncryptionSpace.getElementFromString(v) ).seq
      }
      (shuffled, votes)
    }, "1")*/

    println(s"Verifying shuffle..")
    val ok = Verifier.verifyShuffle(Util.tupleFromSeq(votes), Util.tupleFromSeq(shuffled),
      mix.shuffleProof, proverId, publicKey, in.state.cSettings)
    if(!ok) throw new Exception()
    println(s"Verifying shuffle..Ok")

    new Election[W, Mixing[Succ[T]]](Mixing[Succ[T]](in.state.mixes :+ mix, in.state))
  }

  // stop receiving mixes, can only happen if we have all the mixes
  def stopMixing[W <: Nat](in: Election[W, Mixing[W]]) = {
    println("Mixes done..")
    new Election[W, Mixed](Mixed(in.state))
  }

  // start receiving partial decryptions
  def startDecryptions[W <: Nat](in: Election[W, Mixed]) = {
    println("Now waiting for decryptions")
    new Election[W, Decryptions[_0]](Decryptions[_0](List[PartialDecryptionDTO]().sized(0).get, in.state))
  }

  // verify and add a partial decryption
  def addDecryption[W <: Nat, T <: Nat](in: Election[W, Decryptions[T]], decryption: PartialDecryptionDTO, proverId: String)(implicit ev: T < W) = {
    println("Adding decryption...")

    val elGamal = ElGamalEncryptionScheme.getInstance(in.state.cSettings.generator)
    val votes = in.state.votes.par.map( v => Util.getE(elGamal.getEncryptionSpace, v).asInstanceOf[Pair]).seq

    val sharesMap = in.state.allShares.toMap
    val share = elGamal.getMessageSpace.getElementFrom(sharesMap(proverId))

    val ok = Verifier.verifyPartialDecryption(decryption, votes, in.state.cSettings, proverId, share)
    if(!ok) throw new Exception()

    new Election[W, Decryptions[Succ[T]]](Decryptions[Succ[T]](in.state.decryptions :+ decryption, in.state))
  }

  // combine partial decryptions, can only happen if we have all of them
  def combineDecryptions[W <: Nat](in: Election[W, Decryptions[W]]) = {
    println("Combining decryptions...")
    
    // first convert partial decryptions (a^xi) to elements
    // this yields n lists of decryptions, where n = number of trustees, and there's one decryption per vote
    val decryptionElements = in.state.decryptions.map(
      ds => ds.partialDecryptions.par.map(in.state.cSettings.group.getElementFromString(_)).seq
    )
    // combine the list of decryptions:
    // obtain a^-x from individual a^-xi's (example below for n = 2)
    //
    //      === 1 === === 2 ===
    // v1     a^xi      a^xi      = a^x
    // v2     a^xi      a^xi      = a^x
    // v3     a^xi      a^xi      = a^x
    //  .     a^xi      a^xi      = a^x
    //  .
    //
    val combined = decryptionElements.reduce { (a, b) =>
      (a zip b).par.map(c => c._1.apply(c._2)).seq
    }
    println("Combining decryptions...Ok")

    val elGamal = ElGamalEncryptionScheme.getInstance(in.state.cSettings.generator)
    val votes = in.state.votes.par.map( v => Util.getE(elGamal.getEncryptionSpace, v).asInstanceOf[Pair] ).seq
    // a^-x * b = m
    val decrypted = (votes zip combined).par.map(c => c._1.getSecond().apply(c._2)).seq
    val encoder = ZModPrimeToGStarModSafePrime.getInstance(in.state.cSettings.group)

    new Election[W, Decrypted](Decrypted(decrypted.par.map(encoder.decode(_).convertToString).seq, in.state))
  }
}

/*
 * We use this to generate the entire history for an election.
 * Elections are purely functional, the result is similar to an immutable log
 */
trait HasHistory {
  def prev: ElectionState

  def printHistory(): Unit = {
    println(s"> $this")
    prev match {
      case s1: HasHistory => s1.printHistory
      case s2: ElectionState => println(s"> $s2")
    }
  }
}

/**
 * Some utilities
 */
object Util {
  val unsafe = ConfigFactory.load().getBoolean("use-unsafe-deserialization")

  def tupleFromSeq(items: Seq[Element[_]]) = {
    // var tuple = Tuple.getInstance()
    // items.foreach(v => tuple = tuple.add(v))

    // tuple
    Tuple.getInstance(items:_*)
  }

  def seqFromTuple(tuple: Tuple): Seq[Element[_]] = {

    tuple.par.map{ x => x }.seq.toSeq
  }

  def getRandomVotes(size: Int, generator: Element[_], publicKey: Element[_]) = {
    val elGamal = ElGamalEncryptionScheme.getInstance(generator)

    (1 to size).map { _ =>
      val element = elGamal.getMessageSpace().getRandomElement()
      println(s"* plaintext $element")
      elGamal.encrypt(publicKey, element)
    }
  }

  def encryptVotes(plaintexts: Seq[Int], cSettings: CryptoSettings, publicKey: Element[_]) = {
    val elGamal = ElGamalEncryptionScheme.getInstance(cSettings.generator)
    val encoder = ZModPrimeToGStarModSafePrime.getInstance(cSettings.group)

    plaintexts.par.map { p =>
      val message = encoder.getDomain().getElementFrom(p)
      val encodedMessage = encoder.encode(message)
      elGamal.encrypt(publicKey, encodedMessage)
    }.seq
  }

  def getPublicKeyFromString(publicKey: String, generator: Element[_]) = {
    val elGamal = ElGamalEncryptionScheme.getInstance(generator)
    val keyPairGen = elGamal.getKeyPairGenerator()
    keyPairGen.getPublicKeySpace().getElementFrom(publicKey)
  }

  def getE[A <: Element[B],B](set: AbstractSet[A, B], value: String): Element[B] = {
    set.getElementFromString(value, unsafe)
  }

  // we shouldnt have to have this in addition to the above method, but otherwise the compiler gets confused
  def getE(set: ProductSet, value: String): Element[_] = {
    set.getElementFromString(value, unsafe)
  }
}

/**
 * Convenience election states used to carry information in the election history forward
 */
class ElectionState(val id: String, val cSettings: CryptoSettings)
class ElectionStateShares(id: String, cSettings: CryptoSettings, val allShares: List[(String, String)]) extends ElectionState(id, cSettings)
class ElectionStatePk(id: String, cSettings: CryptoSettings, allShares: List[(String, String)], val publicKey: String) extends ElectionStateShares(id, cSettings, allShares)
class ElectionStateVotes(id: String, cSettings: CryptoSettings, allShares: List[(String, String)], publicKey: String, val votes:List[String]) extends ElectionStatePk(id, cSettings, allShares, publicKey)
