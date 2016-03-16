import shapeless._
import nat._
import syntax.sized._
import ops.nat._
import LT._
import com.github.nscala_time.time.Imports._
import com.typesafe.config.ConfigFactory
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import ch.bfh.unicrypt.math.algebra.multiplicative.classes.GStarModSafePrime
import ch.bfh.unicrypt.crypto.schemes.encryption.classes.ElGamalEncryptionScheme
import ch.bfh.unicrypt.math.algebra.general.interfaces.Element
import ch.bfh.unicrypt.math.algebra.general.classes.Pair
import ch.bfh.unicrypt.math.algebra.general.classes.Tuple
import ch.bfh.unicrypt.crypto.encoder.classes.ZModPrimeToGStarModSafePrime
import ch.bfh.unicrypt.crypto.encoder.interfaces.Encoder
import mpservice.MPBridgeS
import mpservice.MPBridge


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

  val config = ConfigFactory.load()
  val useGmp = config.getBoolean("mpservice.use-gmp")
  val useExtractor = config.getBoolean("mpservice.use-extractor")
  MPBridgeS.init(useGmp, useExtractor)
  // actually used in Util.getE
  val bypass = ConfigFactory.load().getBoolean("bypass-membership-check")
  println(s"* bypass-membership-check: $bypass")
  // actually used in AbstractCyclicGroup constructor
  val generatorsParallel = ConfigFactory.load().getBoolean("use-generators-parallel")
  println(s"* use-generators-parallel: $generatorsParallel")
  // actually used in Util.getIndependentGenerators constructor
  val generatorsParallelLevel = ConfigFactory.load().getInt("generators-parallelism-level")
  println(s"* generators-parallelism-level: $generatorsParallelLevel")

  val totalVotes = args.toList.lift(0).getOrElse("100").toInt

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
  println(votes.length)

  // doing this in one step to avoid memory explosion
  val electionGettingVotes = Election.addVotes(startVotes, votes.map(_.convertToString).toList)

  /*
  // add the votes to the election
  var electionGettingVotes = startVotes

  // this should crash if checking is done in the bb (Election)
  Election.addVotes(electionGettingVotes, "fooooooooooobar")

  votes.foreach { v =>
    electionGettingVotes = Election.addVote(electionGettingVotes, v.convertToString)
  }*/


  // we are only timing the mixing phase
  val mixingStart = System.currentTimeMillis()
  MPBridge.total = 0;
  // wait for keystroke, this allows us to attach a profiler at the right time
  // println("Hit return to start")
  // Console.in.read()

  // stop the voting period
  val stopVotes = Election.stopVotes(electionGettingVotes)

  // we can start preshuffling for both mixers
  // that way the second preshuffle will be concurrent with the first mix
  val (predata1, proof1) = m1.preShuffleVotes(stopVotes)
  val (predata2, proof2) = m2.preShuffleVotes(stopVotes)

  // prepare for mixing
  val startMix = Election.startMixing(stopVotes)

  // each mixing trustee extracts the needed information from the election
  // and performs the shuffle and proofs
  val shuffle1 = m1.shuffleVotes(startMix, predata1, proof1)

  // we compose futures, first mix then second mix
  val mixing = shuffle1.map { shuffle =>
    // the proof is verified and the shuffle is then added to the election, advancing its state
    Election.addMix(startMix, shuffle, m1.id)

  }.flatMap { mixOne =>

    // each mixing trustee extracts the needed information from the election
    // and performs the shuffle and proofs
    m2.shuffleVotes(mixOne, predata2, proof2).map { shuffle =>
      // the proof is verified and the shuffle is then added to the election, advancing its state
      Election.addMix(mixOne, shuffle, m2.id)
    }
  }

  // once all the mixes are finished we proceed to decryption
  val all = mixing.flatMap { mixTwo =>

    // we are done mixing
    val stopMix = Election.stopMixing(mixTwo)

    val mixingEnd = System.currentTimeMillis()

    // start the partial decryptions
    // if we tried to do this before the mixing was completed, the compiler would protest
    val startDecryptions = Election.startDecryptions(stopMix)

    // each keymaker trustee extracts the votes from the last shuffle from the election and
    // uses their private keys to do the partial decryption and create proofs
    val pd1Future = Future { k1.partialDecryption(startDecryptions) }
    val pd2Future = Future { k2.partialDecryption(startDecryptions) }

    // the two decryption futures execute in parallel
    val decryptions = for {
      pd1 <- pd1Future
      pd2 <- pd2Future
    } yield(pd1, pd2)

    decryptions.map { case (pd1, pd2) =>
      val partialOne = Election.addDecryption(startDecryptions, pd1, k1.id)
      val partialTwo = Election.addDecryption(partialOne, pd2, k2.id)

      // the partial decryptions are combined, yielding the plaintexts
      val electionDone = Election.combineDecryptions(partialTwo)

      // lets check that everything went well
      // println(s"Plaintexts $plaintexts")
      // println(s"Decrypted ${electionDone.state.decrypted}")
      // println("ok: " + (plaintexts.sorted == electionDone.state.decrypted.map(_.toInt).sorted))

      val mixTime = (mixingEnd - mixingStart) / 1000.0
      val totalTime = (System.currentTimeMillis() - mixingStart) / 1000.0

      println("*************************************************************")
      println(s"finished run with votes = $totalVotes")
      println(s"mixTime: $mixTime")
      println(s"totalTime: $totalTime")
      println(s"sec / vote (mix): ${mixTime / totalVotes}")
      println(s"sec / vote: ${totalTime / totalVotes}")
      println(s"total modExps: ${MPBridge.total}")
      println(s"found modExps: ${MPBridge.found}")
      println(s"found modExps %: ${MPBridge.found/MPBridge.total.toDouble}")
      println(s"extracted modExps: ${MPBridge.getExtracted}")
      println(s"extracted modExps %: ${MPBridge.getExtracted/MPBridge.total.toDouble}")
      println(s"modExps / vote: ${MPBridge.total.toFloat / totalVotes}")
      println("*************************************************************")

      MPBridgeS.shutdown
    }
  }

  all.onFailure { case e =>
    e.printStackTrace
    MPBridgeS.shutdown
  }
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
    electionGettingVotes = Election.addVote(electionGettingVotes, v.convertToString)
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

object ElectionTestSerial extends App {

  val totalVotes = args.toList.lift(0).getOrElse("100").toInt
  val config = ConfigFactory.load()
  val useGmp = config.getBoolean("mpservice.use-gmp")
  val useExtractor = config.getBoolean("mpservice.use-extractor")
  MPBridgeS.init(useGmp, useExtractor)

  // create the keymakers
  // these are responsible for distributed key generation and joint decryption
  val k1 = new KeyMakerTrustee("keymaker one")
  val k2 = new KeyMakerTrustee("keymaker two")

  // create the mixers
  // these are responsible for shuffling the votes
  val m1 = new MixerTrustee("mixer one")
  val m2 = new MixerTrustee("mixer two")

  // create the election
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
  println(votes.length)

  // add the votes to the election
  var electionGettingVotes = startVotes
  votes.foreach { v =>
    electionGettingVotes = Election.addVote(electionGettingVotes, v.convertToString)
  }

  // we are only timing the mixing phase
  val mixingStart = System.currentTimeMillis()
  MPBridge.total = 0;
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

  // leaving this part out as we want to benchmark only mixing

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

  val mixTime = (mixingEnd - mixingStart) / 1000.0
  val totalTime = (System.currentTimeMillis() - mixingStart) / 1000.0

  println("*************************************************************")
  println(s"finished run with votes = $totalVotes")
  println(s"mixTime: $mixTime")
  println(s"totalTime: $totalTime")
  println(s"sec / vote (mix): ${mixTime / totalVotes}")
  println(s"sec / vote: ${totalTime / totalVotes}")
  println(s"total modExps: ${MPBridge.total}")
  println(s"found modExps: ${MPBridge.found}")
  println(s"found modExps %: ${MPBridge.found/MPBridge.total.toDouble}")
  println(s"extracted modExps: ${MPBridge.getExtracted}")
  println(s"extracted modExps %: ${MPBridge.getExtracted/MPBridge.total.toDouble}")
  println(s"modExps / vote: ${MPBridge.total.toFloat / totalVotes}")
  println("*************************************************************")

  mpservice.MPService.shutdown
}

object DecryptionTest extends App {

  val group = GStarModSafePrime.getFirstInstance(2048)
  val generator = group.getDefaultGenerator()
  val cSettings = CryptoSettings(group, generator)
  val elGamal = ElGamalEncryptionScheme.getInstance(generator)

  object d1 extends KeyMaker
  object d2 extends KeyMaker

  val (e1,pk1) = d1.createShare("d1", cSettings)
  val (e2,pk2) = d2.createShare("d2", cSettings)

  val e1k = Util.getPublicKeyFromString(e1.keyShare, cSettings.generator)
  val e2k = Util.getPublicKeyFromString(e2.keyShare, cSettings.generator)

  val publicKey = e1k.apply(e2k)

  val pk1e = cSettings.group.getZModOrder().getElementFrom(pk1)

  val plaintexts = Seq.fill(300)(scala.util.Random.nextInt(10))
  // encrypt the votes with the public key of the election
  val votes = Util.encryptVotes(plaintexts, cSettings, publicKey)
  println("decrypting..")

  MPBridge.total = 0;

  MPBridge.y()
  val decryption = d1.partialDecrypt(votes, pk1e, "d1", cSettings)
  MPBridge.z()

  MPBridge.y()
  val share = elGamal.getMessageSpace.getElementFrom(e1.keyShare)

  val ok = Verifier.verifyPartialDecryption(decryption, votes, cSettings, "d1", share)
  MPBridge.z()

  MPBridgeS.shutdown
}

object GeneratorTest extends App {
  import ch.bfh.unicrypt.helper.random.deterministic.DeterministicRandomByteSequence
  import ch.bfh.unicrypt.helper.random.deterministic.CTR_DRBG
  import ch.bfh.unicrypt.helper.sequence.Sequence
  import scala.collection.JavaConversions._
  import ch.bfh.unicrypt.helper.converter.classes.biginteger.ByteArrayToBigInteger
  import ch.bfh.unicrypt.helper.math.MathUtil

  val total = 1006
  val split = 10

  val size = total / split
  val remainder = total % split

  val seedLength = CTR_DRBG.getFactory().getSeedByteLength()
  val group = GStarModSafePrime.getFirstInstance(2048)
  val converter = ByteArrayToBigInteger.getInstance(seedLength)
  val d = DeterministicRandomByteSequence.getInstance(CTR_DRBG.getFactory(),
    converter.reconvert(java.math.BigInteger.valueOf(24)))
  val sequence = group.getIndependentGenerators(d).limit(1)
  //val d2 = DeterministicRandomByteSequence.getInstance(CTR_DRBG.getFactory(),
  //  converter.reconvert(java.math.BigInteger.valueOf()))
  // val sequence2 = group.getIndependentGenerators(d2).limit(3)

  println(sequence)
  // println(sequence2)


  /* val a = Array.fill(total % split)((total / split) + 1)
  val b = Array.fill(split - (total % split))(total / split)
  val c = a ++ b

  val seedLength = CTR_DRBG.getFactory().getSeedByteLength()
  val group = GStarModSafePrime.getFirstInstance(2048)
  val converter = ByteArrayToBigInteger.getInstance(seedLength)

  val rds = c.zipWithIndex.map{ case (i, x) =>
    val r = DeterministicRandomByteSequence.getInstance(CTR_DRBG.getFactory(),
    converter.reconvert(java.math.BigInteger.valueOf(x * 1000000)))
    (r, i)
  }
  rds.foreach(println)

  val now1 = System.currentTimeMillis
  val items = rds.par.flatMap { case (d, i) =>
    val sequence = group.getIndependentGenerators(d).limit(i)
    sequence.toList
  }
  println(items.size)
  println(s"${System.currentTimeMillis - now1}")

  // val sequence = group.abstractGetRandomElements(randomByteSequence).skip(0).limit(10000)
  val sequence: Sequence[_ <: Element[_]] = group.getIndependentGenerators().limit(total)
  // val list = mpservice.MPBridgeS.getIndependentGenerators(sequence)
  val now = System.currentTimeMillis
  val items2 = sequence.toList.par.map { x:Element[_] =>
    x
  }
  println(items2.size)
  println(s"${System.currentTimeMillis - now}")*/
}

object Issue4 extends App with ProofSettings {
  import ch.bfh.unicrypt.crypto.keygenerator.interfaces.KeyPairGenerator
  import ch.bfh.unicrypt.crypto.proofsystem.challengegenerator.classes.FiatShamirSigmaChallengeGenerator
  import ch.bfh.unicrypt.crypto.proofsystem.challengegenerator.interfaces.ChallengeGenerator
  import ch.bfh.unicrypt.crypto.proofsystem.challengegenerator.interfaces.SigmaChallengeGenerator
  import ch.bfh.unicrypt.crypto.proofsystem.classes.EqualityPreimageProofSystem
  import ch.bfh.unicrypt.crypto.proofsystem.classes.PermutationCommitmentProofSystem
  import ch.bfh.unicrypt.crypto.proofsystem.classes.PlainPreimageProofSystem
  import ch.bfh.unicrypt.crypto.proofsystem.classes.ReEncryptionShuffleProofSystem
  import ch.bfh.unicrypt.crypto.schemes.commitment.classes.PermutationCommitmentScheme
  import ch.bfh.unicrypt.crypto.schemes.encryption.classes.ElGamalEncryptionScheme
  import ch.bfh.unicrypt.helper.converter.classes.ConvertMethod
  import ch.bfh.unicrypt.helper.converter.classes.biginteger.ByteArrayToBigInteger
  import ch.bfh.unicrypt.helper.converter.classes.bytearray.BigIntegerToByteArray
  import ch.bfh.unicrypt.helper.converter.classes.bytearray.StringToByteArray
  import ch.bfh.unicrypt.helper.hash.HashAlgorithm
  import ch.bfh.unicrypt.helper.hash.HashMethod
  import ch.bfh.unicrypt.helper.math.Alphabet
  import ch.bfh.unicrypt.math.algebra.concatenative.classes.StringElement
  import ch.bfh.unicrypt.math.algebra.concatenative.classes.StringMonoid
  import ch.bfh.unicrypt.math.algebra.general.classes.Pair
  import ch.bfh.unicrypt.math.algebra.general.classes.Triple
  import ch.bfh.unicrypt.math.algebra.general.classes.Tuple
  import ch.bfh.unicrypt.math.algebra.general.interfaces.Element
  import ch.bfh.unicrypt.math.function.classes.CompositeFunction
  import ch.bfh.unicrypt.math.function.classes.GeneratorFunction
  import ch.bfh.unicrypt.math.function.classes.InvertFunction
  import ch.bfh.unicrypt.math.function.classes.MultiIdentityFunction
  import ch.bfh.unicrypt.math.function.classes.ProductFunction
  import ch.bfh.unicrypt.math.function.interfaces.Function
  import ch.bfh.unicrypt.math.algebra.general.abstracts.AbstractSet
  import mpservice.MPBridgeS
  import mpservice.MPBridge

  val group = GStarModSafePrime.getFirstInstance(2048)
  val generator = group.getDefaultGenerator()
  val cSettings = CryptoSettings(group, generator)
  val elGamal = ElGamalEncryptionScheme.getInstance(generator)

  val keyPair = elGamal.getKeyPairGenerator().generateKeyPair()
  val privateKey = keyPair.getFirst()
  val publicKey = keyPair.getSecond()

  val otherInput: StringElement = StringMonoid.getInstance(Alphabet.UNICODE_BMP).getElement("asdasd")
  val challengeGenerator: SigmaChallengeGenerator = FiatShamirSigmaChallengeGenerator.getInstance(
        cSettings.group.getZModOrder(), otherInput, convertMethod, hashMethod, converter)

  val ecg: ChallengeGenerator = PermutationCommitmentProofSystem.createNonInteractiveEValuesGenerator(
        cSettings.group.getZModOrder(), 400000)

  val spg: ReEncryptionShuffleProofSystem = ReEncryptionShuffleProofSystem.getInstance(challengeGenerator, ecg, 400000, elGamal, publicKey)

  val commitment = scala.io.Source.fromFile("commitment.dat").mkString
  val commitment2 = spg.getCommitmentSpace().asInstanceOf[AbstractSet[_,_]].getElementFrom(commitment)
}

object Issue3 extends App {
  val grp = GStarModSafePrime.getInstance(167)
  // val grp = GStarModSafePrime.getInstance(new BigInteger("170141183460469231731687303715884114527"))
  // val grp = GStarModSafePrime.getFirstInstance(2048)
  val gen = grp.getDefaultGenerator()
  val Csettings = CryptoSettings(grp, gen)
  val elGamal = ElGamalEncryptionScheme.getInstance(Csettings.generator)

  val keyPair = elGamal.getKeyPairGenerator().generateKeyPair()
  val privateKey = keyPair.getFirst()
  val publicKey = keyPair.getSecond()

  // eventually 0 will be used in Z_q
  val votes = Util.encryptVotes(List(0, 1, 2), Csettings, publicKey)
  votes.foreach { v =>
    val first = v.getFirst
    println(first)
    println(v.getFirst.isGenerator)
    val decryption = elGamal.decrypt(privateKey, v)
    println("decrypted " + decryption)
  }
}