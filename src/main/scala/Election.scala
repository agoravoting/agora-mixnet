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

class ElectionState(val id: String, val cSettings: CryptoSettings)
class ElectionStateShares(id: String, cSettings: CryptoSettings, val allShares: List[(String, String)]) extends ElectionState(id, cSettings)
class ElectionStatePk(id: String, cSettings: CryptoSettings, allShares: List[(String, String)], val publicKey: String) extends ElectionStateShares(id, cSettings, allShares)
class ElectionStateVotes(id: String, cSettings: CryptoSettings, allShares: List[(String, String)], publicKey: String, val votes:List[String]) extends ElectionStatePk(id, cSettings, allShares, publicKey)

case class Created(override val id: String, override val cSettings: CryptoSettings) extends ElectionState(id, cSettings)
case class Shares[T <: Nat](val shares: Sized[List[(String, String)], T], prev: ElectionState) extends ElectionStateShares(prev.id, prev.cSettings, shares.toList) with HasHistory
case class Combined(override val publicKey: String, prev: ElectionStateShares) extends ElectionStatePk(prev.id, prev.cSettings, prev.allShares, publicKey) with HasHistory
case class Votes(votes: List[String], prev: ElectionStatePk) extends ElectionStatePk(prev.id, prev.cSettings, prev.allShares, prev.publicKey) with HasHistory
case class VotesStopped(prev: Votes, date: DateTime = DateTime.now) extends ElectionStateVotes(prev.id, prev.cSettings, prev.allShares, prev.publicKey, prev.votes) with HasHistory
case class Mixing[T <: Nat](mixes: Sized[List[ShuffleResultDTO], T], prev: ElectionStateVotes) extends ElectionStateVotes(prev.id, prev.cSettings, prev.allShares, prev.publicKey, prev.votes) with HasHistory
case class Mixed(prev: Mixing[_ <: Nat]) extends ElectionStateVotes(prev.id, prev.cSettings, prev.allShares, prev.publicKey, prev.votes) with HasHistory
case class Decryptions[T <: Nat](decryptions: Sized[List[PartialDecryptionDTO], T], prev: ElectionStateVotes) extends ElectionStateVotes(prev.id, prev.cSettings, prev.allShares, prev.publicKey, prev.votes) with HasHistory
case class Decrypted(decrypted: List[String], prev: Decryptions[_ <: Nat]) extends ElectionStateVotes(prev.id, prev.cSettings, prev.allShares, prev.publicKey, prev.votes) with HasHistory

class Election[+W <: Nat, +S <: ElectionState] private (val state: S) {
  override def toString() = s"election ${state.id}, ${state.toString}"
}

object Election {

  def start[W <: Nat](id: String, bits: Int) = {
    println("Going to start a new Election!")
    val group = GStarModSafePrime.getFirstInstance(bits)
    val generator = group.getDefaultGenerator()
    val cSettings = CryptoSettings(group, generator)

    new Election[W, Created](Created(id, cSettings))
  }

  def startShares[W <: Nat](in: Election[W, Created]) = {
    println("Now waiting for shares")
    new Election[W, Shares[_0]](Shares[_0](List[(String, String)]().sized(0).get, in.state))
  }

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

  def startVotes[W <: Nat](in: Election[W, Combined]) = {
    println("Now waiting for votes")
    new Election[W, Votes](Votes(List[String](), in.state))
  }

  def addVotes[W <: Nat](in: Election[W, Votes], vote: String) = {
    println("Adding vote..")
    new Election[W, Votes](Votes(vote :: in.state.votes, in.state))
  }

  def stopVotes[W <: Nat](in: Election[W, Votes]) = {
    println("No more votes")
    new Election[W, VotesStopped](VotesStopped(in.state))
  }

  def startMixing[W <: Nat](in: Election[W, VotesStopped]) = {
    println("Now waiting for mixes")
    new Election[W, Mixing[_0]](Mixing[_0](List[ShuffleResultDTO]().sized(0).get, in.state))
  }

  def addMix[W <: Nat, T <: Nat](in: Election[W, Mixing[T]], mix: ShuffleResultDTO, proverId: String)(implicit ev: T < W) = {
    println("Adding mix...")
    val elGamal = ElGamalEncryptionScheme.getInstance(in.state.cSettings.generator)
    val keyPairGen = elGamal.getKeyPairGenerator()
    val publicKey = keyPairGen.getPublicKeySpace().getElementFrom(in.state.publicKey)
    val shuffled = mix.votes.map( v => elGamal.getEncryptionSpace.getElementFrom(v) )
    val votes = in.state match {
      case s: Mixing[_0] => in.state.votes.map( v => elGamal.getEncryptionSpace.getElementFrom(v) )
      case _ => in.state.mixes.toList.last.votes.map( v => elGamal.getEncryptionSpace.getElementFrom(v) )
    }
    mix.votes.map( v => elGamal.getEncryptionSpace.getElementFrom(v) )

    println(s"verifying shuffle from $votes to $mix")
    val ok = Verifier.verifyShuffle(Util.tupleFromSeq(votes), Util.tupleFromSeq(shuffled),
      mix.shuffleProof, proverId, publicKey, in.state.cSettings)
    if(!ok) throw new Exception()

    new Election[W, Mixing[Succ[T]]](Mixing[Succ[T]](in.state.mixes :+ mix, in.state))
  }

  def stopMixing[W <: Nat](in: Election[W, Mixing[W]]) = {
    println("Mixes done..")
    new Election[W, Mixed](Mixed(in.state))
  }

  def startDecryptions[W <: Nat](in: Election[W, Mixed]) = {
    println("Now waiting for decryptions")
    new Election[W, Decryptions[_0]](Decryptions[_0](List[PartialDecryptionDTO]().sized(0).get, in.state))
  }

  def addDecryption[W <: Nat, T <: Nat](in: Election[W, Decryptions[T]], decryption: PartialDecryptionDTO, proverId: String)(implicit ev: T < W) = {
    println("Adding decryption...")

    val elGamal = ElGamalEncryptionScheme.getInstance(in.state.cSettings.generator)
    val votes = in.state.votes.map( v => elGamal.getEncryptionSpace.getElementFrom(v))

    val sharesMap = in.state.allShares.toMap
    val share = elGamal.getMessageSpace.getElementFrom(sharesMap(proverId))

    val ok = Verifier.verifyPartialDecryptions(decryption, votes, in.state.cSettings, proverId, share)
    if(!ok) throw new Exception()

    new Election[W, Decryptions[Succ[T]]](Decryptions[Succ[T]](in.state.decryptions :+ decryption, in.state))
  }

  def combineDecryptions[W <: Nat](in: Election[W, Decryptions[W]]) = {
    println("Combining decryptions...")


    val combined = in.state.decryptions.map( x => x.partialDecryptions ).reduce { (a, b) =>
      (a zip b).map(c => c._1.apply(c._2))
    }
    println(s"a^-x $combined")

    val elGamal = ElGamalEncryptionScheme.getInstance(in.state.cSettings.generator)
    val votes = in.state.votes.map( v => elGamal.getEncryptionSpace.getElementFrom(v).asInstanceOf[Pair] )
    // a^-x * b = m
    val decrypted = (votes zip combined).map(c => c._1.getSecond().apply(c._2))


    new Election[W, Decrypted](Decrypted(decrypted.map(_.convertToString), in.state))
  }
}

object ElectionTest extends App {
  val k1 = KeyMakerTrustee("keymaker one")
  val k2 = KeyMakerTrustee("keymaker two")
  val m1 = MixerTrustee("mixer one")
  val m2 = MixerTrustee("mixer two")

  val start = Election.start[_2]("my election", 8)

  val readyForShares = Election.startShares(start)

  val oneShare = Election.addShare(readyForShares, k1.createShare(readyForShares), k1.id)
  val twoShares = Election.addShare(oneShare, k2.createShare(readyForShares), k2.id)
  val combined = Election.combineShares(twoShares)
  val publicKey = Util.getPublicKeyFromString(combined.state.publicKey, combined.state.cSettings.generator)

  val startVotes = Election.startVotes(combined)
  val votes = Util.getRandomVotes(5, combined.state.cSettings.generator, publicKey)
  var electionGettingVotes = startVotes
  votes.foreach { v =>
    electionGettingVotes = Election.addVotes(electionGettingVotes, v.convertToString)
  }
  val stopVotes = Election.stopVotes(electionGettingVotes)

  val startMix = Election.startMixing(stopVotes)
  val shuffle1 = m1.shuffle(startMix)
  val mixOne = Election.addMix(startMix, shuffle1, m1.id)
  val shuffle2 = m1.shuffle(mixOne)
  val mixTwo = Election.addMix(mixOne, shuffle2, m1.id)
  val stopMix = Election.stopMixing(mixTwo)

  val startDecryptions = Election.startDecryptions(stopMix)
  val pd1 = k1.partialDecrypt(startDecryptions)
  val pd2 = k2.partialDecrypt(startDecryptions)
  val partialOne = Election.addDecryption(startDecryptions, pd1, k1.id)
  val partialTwo = Election.addDecryption(partialOne, pd2, k2.id)

  val electionDone = Election.combineDecryptions(partialTwo)
  println("===== Election Dump =====")
  println(electionDone)
  println("===== Election Dump =====")
  println(s"Decrypted votes ${electionDone.state.decrypted}")
}

case class KeyMakerTrustee(id: String, privateShares: scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map()) {
  def createShare(e: Election[_, Shares[_]]) = {
    val (encryptionKeyShareDTO, privateKey) = KeyMaker.createShare(id, e.state.cSettings)
    privateShares += (e.state.id -> privateKey)
    encryptionKeyShareDTO
  }

  def partialDecrypt(e: Election[_, Decryptions[_]]) = {
    val elGamal = ElGamalEncryptionScheme.getInstance(e.state.cSettings.generator)
    val votes = e.state.votes.map( v => elGamal.getEncryptionSpace.getElementFrom(v))
    val secretKey = e.state.cSettings.group.getZModOrder().getElementFrom(privateShares(e.state.id))

    KeyMaker.partialDecrypt(votes, secretKey.convertToBigInteger, id, e.state.cSettings)
  }
}

case class MixerTrustee(id: String) {
  def shuffle(e: Election[_, Mixing[_]]) = {
    val elGamal = ElGamalEncryptionScheme.getInstance(e.state.cSettings.generator)
    val keyPairGen = elGamal.getKeyPairGenerator()
    val publicKey = keyPairGen.getPublicKeySpace().getElementFrom(e.state.publicKey)
    val votes = e.state match {
      case s: Mixing[_0] => e.state.votes.map( v => elGamal.getEncryptionSpace.getElementFrom(v) )
      case _ => e.state.mixes.toList.last.votes.map( v => elGamal.getEncryptionSpace.getElementFrom(v) )
    }
    Mixer.shuffle(Util.tupleFromSeq(votes), publicKey, e.state.cSettings, id)
  }
}