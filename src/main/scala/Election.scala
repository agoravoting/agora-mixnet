import shapeless._
import nat._
import syntax.sized._
import ops.nat._
import LT._
import com.github.nscala_time.time.Imports._

import ch.bfh.unicrypt.math.algebra.multiplicative.classes.GStarModSafePrime
import ch.bfh.unicrypt.crypto.schemes.encryption.classes.ElGamalEncryptionScheme
import ch.bfh.unicrypt.math.algebra.general.interfaces.Element
import ch.bfh.unicrypt.math.algebra.general.classes.Pair
import ch.bfh.unicrypt.math.algebra.general.classes.Tuple
import ch.bfh.unicrypt.crypto.encoder.classes.ZModPrimeToGStarModSafePrime
import ch.bfh.unicrypt.crypto.encoder.interfaces.Encoder

/**
 * An election process DEMO
 *
 * Simulates the steps in the election from public key generation all the way to decryption
 *
 * Things that are included in this demo are:
 *
 * - A typed purely functional data structure modeling the election process and bulletin board (see below)
 *
 * - Cryptography for
 *
 *  a) encrypting votes
 *  b) creating keyshares, proofs and verification
 *  c) shuffling votes, proofs and verification
 *  d) joint (partial) decryption, proofs and verification
 *
 * - Not included
 *
 * Remoting (everything simulated with method calls)
 * Signatures and authentication
 * Error handling
 * Proofs of knowledge of plaintext and verification in vote casting
 *
 *
 * An election is modeled as a typed, purely functional sequential state machine. We use shapeless
 * encoding of natural numbers to provide length-typed lists (aka dependent types), that way we get:
 *
 * 1) The election process logic is captured by types, so illegal transitions
 *    are caught by the compiler and inconsistent states are not possible, for example
 *
 *    It is a compile-time error to try to construct the public key without all the shares
 *    It is a compile-time error to add more shares,shuffles or decryptions than expected
 *    It is a compile-error to start an election with no public key
 *    It is a compile-time error to decrypt without shuffling
 *    etc.
 *
 * 2) Because the election is purely functional, the entire history of the election
 *    can be reconstructed or replayed. A purely functional data structure is in this
 *    sense a general case of an immutable log
 *
 *
 * This demo uses two trustees, ElectionTest3 below shows how number of trustees generalizes
 */
object ElectionTest extends App {
  import ch.bfh.unicrypt.math.algebra.multiplicative.classes.GStarMod

  val totalVotes = args.toList.lift(0).getOrElse("100").toInt
  val gmp = args.toList.lift(1).getOrElse("") == "gmp"
  GStarMod.gmpModPow = gmp

  // create the keymakers
  // these are responsible for distributed key generation and joint decryption
  val k1 = new KeyMakerTrustee("keymaker one")
  val k2 = new KeyMakerTrustee("keymaker two")

  // create the mixers
  // these are responsible for shuffling the votes
  val m1 = new MixerTrustee("mixer one")
  val m2 = new MixerTrustee("mixer two")

  // create the election,
  // we are using privacy level 2, two trustees of each kind
  // we are 2048 bits for the size of the group modulus
  val start = Election.create[_2]("my election", 2048)

  // the election is now ready to receive key shares
  val readyForShares = Election.startShares(start)

  // each keymaker creates the shares and their proofs, these are added to the election
  val oneShare = Election.addShare(readyForShares, k1.createKeyShare(readyForShares), k1.id)
  val twoShares = Election.addShare(oneShare, k2.createKeyShare(readyForShares), k2.id)

  // combine the shares from the keymaker trustees, this produces the election public key
  val combined = Election.combineShares(twoShares)

  // since we are storing information in election as if it were a bulletin board, all
  // the data is stored in a wire-compatible format, that is strings/jsons whatever
  // we reconstruct the public key as if it had been read from such a format
  val publicKey = Util.getPublicKeyFromString(combined.state.publicKey, combined.state.cSettings.generator)

  // open the election period
  val startVotes = Election.startVotes(combined)

  // generate dummy votes
  val plaintexts = Seq.fill(totalVotes)(scala.util.Random.nextInt(10))

  // encrypt the votes with the public key of the election
  val votes = Util.encryptVotes(plaintexts, combined.state.cSettings, publicKey)

  // add the votes to the election
  var electionGettingVotes = startVotes
  votes.foreach { v =>
    electionGettingVotes = Election.addVotes(electionGettingVotes, v.convertToString)
  }

  // we are only timing the mixing phase
  val mixingStart = System.currentTimeMillis()
  GStarMod.modExps = 0;
  
  // wait for keystroke, this allows us to attach a profiler at the right time
  // println("Hit return to start")
  // Console.in.read()

  // stop the voting period
  val stopVotes = Election.stopVotes(electionGettingVotes)

  // prepare for mixing
  val startMix = Election.startMixing(stopVotes)

  // each mixing trustee extracts the needed information from the election
  // and performs the shuffle and proofs
  val shuffle1 = m1.shuffleVotes(startMix)

  // the proof is verified and the shuffle is then added to the election, advancing its state
  val mixOne = Election.addMix(startMix, shuffle1, m1.id)

  // again for the second trustee..
  val shuffle2 = m2.shuffleVotes(mixOne)
  val mixTwo = Election.addMix(mixOne, shuffle2, m2.id)

  // we are done mixing
  val stopMix = Election.stopMixing(mixTwo)

  val mixingEnd = System.currentTimeMillis()
  val modExps = GStarMod.modExps

  // leaving this part out as we want to benchmark only mixing
  /*
  // start the partial decryptions
  // if we tried to do this before the mixing was completed, the compiler would protest
  val startDecryptions = Election.startDecryptions(stopMix)

  // each keymaker trustee extracts the votes from the last shuffle from the election and
  // uses their private keys to do the partial decryption and create proofs
  val pd1 = k1.partialDecryption(startDecryptions)
  val pd2 = k2.partialDecryption(startDecryptions)

  // the proofs are verified and the partial decryptions are added to the election,
  val partialOne = Election.addDecryption(startDecryptions, pd1, k1.id)
  val partialTwo = Election.addDecryption(partialOne, pd2, k2.id)

  // the partial decryptions are combined, yielding the plaintexts
  val electionDone = Election.combineDecryptions(partialTwo)

  // lets check that everything went well
  println(s"Plaintexts $plaintexts")
  println(s"Decrypted ${electionDone.state.decrypted}")
  println("ok: " + (plaintexts.sorted == electionDone.state.decrypted.map(_.toInt).sorted))

  */

  val mixTime = (mixingEnd - mixingStart) / 1000.0

  println("*************************************************************")
  println(s"finished run with votes = $totalVotes")
  println(s"mixTime: $mixTime")
  println(s"sec / vote: ${mixTime / totalVotes}")
  println(s"modExps: $modExps")
  println(s"modExps / vote: ${modExps.toFloat / totalVotes}")
  println("*************************************************************")
}

/**
 * Same as above but with three trustees
 *
 * Note that everything is done the same way except the type parameter _3 and
 * the number of trustee operations
 *
 */
object ElectionTest3 extends App {
  val totalVotes = args.toList.headOption.getOrElse("100").toInt

  val k1 = new KeyMakerTrustee("keymaker one")
  val k2 = new KeyMakerTrustee("keymaker two")
  val k3 = new KeyMakerTrustee("keymaker three")

  val m1 = new MixerTrustee("mixer one")
  val m2 = new MixerTrustee("mixer two")
  val m3 = new MixerTrustee("mixer three")

  // privacy level 3, three trustees of each kind, 512 bits for the size of the group modulus
  val start = Election.create[_3]("my election", 512)

  val readyForShares = Election.startShares(start)

  val oneShare = Election.addShare(readyForShares, k1.createKeyShare(readyForShares), k1.id)
  val twoShares = Election.addShare(oneShare, k2.createKeyShare(readyForShares), k2.id)
  val threeShares = Election.addShare(twoShares, k3.createKeyShare(readyForShares), k3.id)

  val combined = Election.combineShares(threeShares)

  val publicKey = Util.getPublicKeyFromString(combined.state.publicKey, combined.state.cSettings.generator)

  val startVotes = Election.startVotes(combined)

  val plaintexts = Seq.fill(totalVotes)(scala.util.Random.nextInt(10))

  val votes = Util.encryptVotes(plaintexts, combined.state.cSettings, publicKey)

  var electionGettingVotes = startVotes
  votes.foreach { v =>
    electionGettingVotes = Election.addVotes(electionGettingVotes, v.convertToString)
  }

  val stopVotes = Election.stopVotes(electionGettingVotes)

  val startMix = Election.startMixing(stopVotes)

  val shuffle1 = m1.shuffleVotes(startMix)
  val mixOne = Election.addMix(startMix, shuffle1, m1.id)
  val shuffle2 = m2.shuffleVotes(mixOne)
  val mixTwo = Election.addMix(mixOne, shuffle2, m2.id)
  val shuffle3 = m3.shuffleVotes(mixTwo)
  val mixThree = Election.addMix(mixTwo, shuffle3, m3.id)

  val stopMix = Election.stopMixing(mixThree)

  val startDecryptions = Election.startDecryptions(stopMix)

  val pd1 = k1.partialDecryption(startDecryptions)
  val pd2 = k2.partialDecryption(startDecryptions)
  val pd3 = k3.partialDecryption(startDecryptions)

  val partialOne = Election.addDecryption(startDecryptions, pd1, k1.id)
  val partialTwo = Election.addDecryption(partialOne, pd2, k2.id)
  val partialThree = Election.addDecryption(partialTwo, pd3, k3.id)

  val electionDone = Election.combineDecryptions(partialThree)

  println(s"Plaintexts $plaintexts")
  println(s"Decrypted ${electionDone.state.decrypted}")
  println("ok: " + (plaintexts.sorted == electionDone.state.decrypted.map(_.toInt).sorted))
}

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
case class Decrypted(decrypted: List[String], prev: Decryptions[_ <: Nat]) extends ElectionStateVotes(prev.id, prev.cSettings, prev.allShares, prev.publicKey, prev.votes) with HasHistory

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
    println(s"Adding share... $share")

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
    var encKey = in.state.cSettings.group.getIdentityElement()

    val shares = in.state.shares.map { s =>
      Util.getPublicKeyFromString(s._2, in.state.cSettings.generator)
    }
    val publicKey = shares.reduce( (a,b) => a.apply(b) )

    println(s"combineShares: public key $publicKey")
    encKey

    new Election[W, Combined](Combined(publicKey.convertToString, in.state))
  }

  // start the voting period
  def startVotes[W <: Nat](in: Election[W, Combined]) = {
    println("Now waiting for votes")
    new Election[W, Votes](Votes(List[String](), in.state))
  }

  // votes are casted here
  def addVotes[W <: Nat](in: Election[W, Votes], vote: String) = {
    print("+")
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
    val shuffled = mix.votes.map( v => elGamal.getEncryptionSpace.getElementFromString(v) )
    val votes = in.state match {
      case s: Mixing[_0] => in.state.votes.map( v => elGamal.getEncryptionSpace.getElementFromString(v) )
      case _ => in.state.mixes.toList.last.votes.map( v => elGamal.getEncryptionSpace.getElementFromString(v) )
    }
    mix.votes.map( v => elGamal.getEncryptionSpace.getElementFromString(v) )

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
    val votes = in.state.votes.map( v => elGamal.getEncryptionSpace.getElementFromString(v).asInstanceOf[Pair])

    val sharesMap = in.state.allShares.toMap
    val share = elGamal.getMessageSpace.getElementFrom(sharesMap(proverId))

    val ok = Verifier.verifyPartialDecryptions(decryption, votes, in.state.cSettings, proverId, share)
    if(!ok) throw new Exception()

    new Election[W, Decryptions[Succ[T]]](Decryptions[Succ[T]](in.state.decryptions :+ decryption, in.state))
  }

  // combine partial decryptions, can only happen if we have all of them
  def combineDecryptions[W <: Nat](in: Election[W, Decryptions[W]]) = {
    println("Combining decryptions...")

    // obtain a^-x from individual a^-xi's
    val combined = in.state.decryptions.map( x => x.partialDecryptions ).reduce { (a, b) =>
      (a zip b).map(c => c._1.apply(c._2))
    }
    println("Combining decryptions...Ok")

    val elGamal = ElGamalEncryptionScheme.getInstance(in.state.cSettings.generator)
    val votes = in.state.votes.map( v => elGamal.getEncryptionSpace.getElementFromString(v).asInstanceOf[Pair] )
    // a^-x * b = m
    val decrypted = (votes zip combined).map(c => c._1.getSecond().apply(c._2))
    val encoder = ZModPrimeToGStarModSafePrime.getInstance(in.state.cSettings.group)

    new Election[W, Decrypted](Decrypted(decrypted.map(encoder.decode(_).convertToString), in.state))
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
  def tupleFromSeq(items: Seq[Element[_]]) = {
    var tuple = Tuple.getInstance()
    items.foreach(v => tuple = tuple.add(v))

    tuple
  }

  def seqFromTuple(tuple: Tuple): Seq[Element[_]] = {
    import scala.collection.JavaConversions._

    tuple.map{ x => x }.toSeq
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

    plaintexts.map { p =>
      val message = encoder.getDomain().getElementFrom(p)
      val encodedMessage = encoder.encode(message)
      elGamal.encrypt(publicKey, encodedMessage)
    }
  }

  def getPublicKeyFromString(publicKey: String, generator: Element[_]) = {
    val elGamal = ElGamalEncryptionScheme.getInstance(generator)
    val keyPairGen = elGamal.getKeyPairGenerator()
    keyPairGen.getPublicKeySpace().getElementFrom(publicKey)
  }
}

/**
 * Convenience election states used to carry information in the election history forward
 */
class ElectionState(val id: String, val cSettings: CryptoSettings)
class ElectionStateShares(id: String, cSettings: CryptoSettings, val allShares: List[(String, String)]) extends ElectionState(id, cSettings)
class ElectionStatePk(id: String, cSettings: CryptoSettings, allShares: List[(String, String)], val publicKey: String) extends ElectionStateShares(id, cSettings, allShares)
class ElectionStateVotes(id: String, cSettings: CryptoSettings, allShares: List[(String, String)], publicKey: String, val votes:List[String]) extends ElectionStatePk(id, cSettings, allShares, publicKey)