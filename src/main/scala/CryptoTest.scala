import java.math.BigInteger

import ch.bfh.unicrypt.crypto.proofsystem.challengegenerator.interfaces.ChallengeGenerator
import ch.bfh.unicrypt.crypto.proofsystem.challengegenerator.interfaces.SigmaChallengeGenerator
import ch.bfh.unicrypt.crypto.proofsystem.classes.PermutationCommitmentProofSystem
import ch.bfh.unicrypt.crypto.proofsystem.classes.ReEncryptionShuffleProofSystem
import ch.bfh.unicrypt.crypto.schemes.commitment.classes.PermutationCommitmentScheme
import ch.bfh.unicrypt.crypto.schemes.encryption.classes.ElGamalEncryptionScheme
import ch.bfh.unicrypt.crypto.schemes.encryption.interfaces.ReEncryptionScheme
import ch.bfh.unicrypt.helper.math.Alphabet
import ch.bfh.unicrypt.helper.math.Permutation
import ch.bfh.unicrypt.math.algebra.additive.classes.ECZModPrime
import ch.bfh.unicrypt.math.algebra.concatenative.classes.StringMonoid
import ch.bfh.unicrypt.math.algebra.dualistic.classes.ZMod
import ch.bfh.unicrypt.math.algebra.general.classes.Pair
import ch.bfh.unicrypt.math.algebra.general.classes.PermutationElement
import ch.bfh.unicrypt.math.algebra.general.classes.PermutationGroup
import ch.bfh.unicrypt.math.algebra.general.classes.ProductGroup
import ch.bfh.unicrypt.math.algebra.general.classes.Triple
import ch.bfh.unicrypt.math.algebra.general.classes.Tuple
import ch.bfh.unicrypt.math.algebra.general.interfaces.Element
import ch.bfh.unicrypt.math.algebra.general.interfaces.Group
import ch.bfh.unicrypt.math.algebra.multiplicative.classes.GStarMod
import ch.bfh.unicrypt.math.algebra.multiplicative.classes.GStarModSafePrime
import ch.bfh.unicrypt.crypto.proofsystem.challengegenerator.classes.FiatShamirSigmaChallengeGenerator
import ch.bfh.unicrypt.helper.converter.classes.ConvertMethod
import ch.bfh.unicrypt.helper.converter.classes.biginteger.ByteArrayToBigInteger
import ch.bfh.unicrypt.helper.converter.classes.bytearray.BigIntegerToByteArray
import ch.bfh.unicrypt.helper.converter.classes.bytearray.StringToByteArray
import ch.bfh.unicrypt.helper.converter.interfaces.Converter
import ch.bfh.unicrypt.helper.hash.HashAlgorithm
import ch.bfh.unicrypt.helper.hash.HashMethod
import ch.bfh.unicrypt.math.algebra.concatenative.classes.StringElement
import ch.bfh.unicrypt.math.algebra.concatenative.classes.StringMonoid
import ch.bfh.unicrypt.crypto.proofsystem.classes.PlainPreimageProofSystem
import ch.bfh.unicrypt.crypto.proofsystem.classes.EqualityPreimageProofSystem
import ch.bfh.unicrypt.math.algebra.params.classes.SECECCParamsFp
import ch.bfh.unicrypt.math.function.classes.PermutationFunction
import ch.bfh.unicrypt.random.classes.CounterModeRandomByteSequence
import ch.bfh.unicrypt.random.classes.PseudoRandomOracle
import ch.bfh.unicrypt.random.classes.ReferenceRandomByteSequence
import ch.bfh.unicrypt.random.interfaces.RandomByteSequence
import ch.bfh.unicrypt.random.interfaces.RandomOracle
import ch.bfh.unicrypt.math.algebra.general.interfaces.CyclicGroup
import ch.bfh.unicrypt.crypto.mixer.classes.ReEncryptionMixer
import ch.bfh.unicrypt.math.algebra.multiplicative.classes.GStarModElement
import ch.bfh.unicrypt.crypto.keygenerator.interfaces.KeyPairGenerator
import ch.bfh.unicrypt.math.function.classes.GeneratorFunction
import ch.bfh.unicrypt.math.function.classes.CompositeFunction
import ch.bfh.unicrypt.math.function.classes.GeneratorFunction
import ch.bfh.unicrypt.math.function.classes.InvertFunction
import ch.bfh.unicrypt.math.function.classes.MultiIdentityFunction
import ch.bfh.unicrypt.math.function.classes.ProductFunction
import ch.bfh.unicrypt.math.function.interfaces.Function
import java.nio.ByteOrder
import java.nio.charset.Charset

// case class CryptoSettings(group: Group[_], generator: Element[_])
case class CryptoSettings(group: GStarModSafePrime, generator: Element[_])

case class EncryptionKeyShareDTO(sigmaProofDTO: SigmaProofDTO, keyShare: String)
case class PartialDecryptionDTO(partialDecryptions: Seq[Element[_]], proofDTO: SigmaProofDTO)
case class SigmaProofDTO(commitment: String, challenge: String, response: String)

case class ShuffleResultDTO(shuffleProof: ShuffleProofDTO, votes: Seq[String])
case class PermutationProofDTO(commitment: String, challenge: String, response: String,
  bridingCommitments: Seq[String], eValues: Seq[String])
case class MixProofDTO(commitment: String, challenge: String, response: String, eValues: Seq[String])
case class ShuffleProofDTO(mixProof: MixProofDTO, permutationProof: PermutationProofDTO, permutationCommitment: String)


object CryptoTest extends App {

  val grp = GStarModSafePrime.getInstance(167)
  // val grp = GStarModSafePrime.getInstance(new BigInteger("170141183460469231731687303715884114527"))
  // val grp = GStarModSafePrime.getFirstInstance(1024)

  val gen = grp.getDefaultGenerator()
  val Csettings = CryptoSettings(grp, gen)

  val shares = scala.collection.mutable.ArrayBuffer.empty[Element[_]]
  val privates = scala.collection.mutable.ArrayBuffer.empty[Element[_]]

  testDkgAndJointDecryption()
  testShuffle()

  def testShuffle() = {
    val elGamal = ElGamalEncryptionScheme.getInstance(Csettings.generator)
    val keyPair = elGamal.getKeyPairGenerator().generateKeyPair()
    val privateKey = keyPair.getFirst()
    val publicKey = keyPair.getSecond()
    val votes = Util.getRandomVotes(10, Csettings.generator, publicKey)

    val shuffleResult = Mixer.shuffle(Util.tupleFromSeq(votes), publicKey, Csettings, "proverId")
    val shuffled = shuffleResult.votes.map( v => elGamal.getEncryptionSpace.getElementFrom(v) )

    Verifier.verifyShuffle(Util.tupleFromSeq(votes), Util.tupleFromSeq(shuffled),
      shuffleResult.shuffleProof, "proverId", publicKey, Csettings)

    shuffled.foreach { v =>
      val decryption = elGamal.decrypt(privateKey, v)
      println("decrypted " + decryption)
    }
  }

  def testDkgAndJointDecryption() = {
    val (share, key) = KeyMaker.createShare("1", Csettings)
    addShare(share, "1", Csettings, key)
    val (share2, key2) = KeyMaker.createShare("2", Csettings)
    addShare(share2, "2", Csettings, key2)
    println(s"Shares $shares")
    val publicKey = combineShares(shares, Csettings)

    val ciphertexts = Util.getRandomVotes(10, Csettings.generator, publicKey)

    // a^-x1
    val elementsOne = KeyMaker.partialDecrypt(ciphertexts, privates(0).convertToBigInteger, "0", Csettings)
    var ok = Verifier.verifyPartialDecryptions(elementsOne, ciphertexts, Csettings, "0", shares(0))
    if(!ok) throw new Exception()
    // a^-x2
    val elementsTwo = KeyMaker.partialDecrypt(ciphertexts, privates(1).convertToBigInteger, "1", Csettings)
    ok = Verifier.verifyPartialDecryptions(elementsTwo, ciphertexts, Csettings, "1", shares(1))
    if(!ok) throw new Exception()

    println(s"partial decrypts one ****\n$elementsOne")
    println(s"partial decrypts two ****\n $elementsTwo")
    // a^-x = a^-x1 * a^-x2 ...
    val combined = (elementsOne.partialDecryptions zip elementsTwo.partialDecryptions).map(c => c._1.apply(c._2))
    println(s"a^-x ****\n$combined")
    // a^-x * b = m
    val decrypted = (ciphertexts zip combined).map(c => c._1.getSecond().apply(c._2))
    println(s"Decrypted $decrypted")
  }

  def combineShares(shares: Seq[Element[_]], Csettings: CryptoSettings) = {
    var encKey = Csettings.group.getIdentityElement()

    // y = y1 * y2 * y3....
    for (keyShare <- shares) {
      encKey = encKey.apply(keyShare)
    }

    println(s"combineShares: public key $encKey")
    encKey
  }

  def addShare(encryptionKeyShare: EncryptionKeyShareDTO, proverId: String, CSettings: CryptoSettings, privateK: String) = {
    val result = Verifier.verifyKeyShare(encryptionKeyShare, Csettings, proverId: String)
    if(result) {
      val elGamal = ElGamalEncryptionScheme.getInstance(Csettings.generator)
      val keyPairGen: KeyPairGenerator = elGamal.getKeyPairGenerator()
      val publicKey = keyPairGen.getPublicKeySpace().getElementFrom(encryptionKeyShare.keyShare)
      shares += publicKey
      val privateKey = keyPairGen.getPrivateKeySpace().getElementFrom(privateK)

      privates += privateKey
      println(s"Share added $publicKey $privateKey")
    }
    else {
      //Remove tallier
      throw new Exception("********** Share failed verification")
    }
  }
}

object KeyMaker extends ProofSettings {

  def createShare(proverId: String, Csettings: CryptoSettings) = {

    val elGamal = ElGamalEncryptionScheme.getInstance(Csettings.generator)

    val kpg = elGamal.getKeyPairGenerator()
    val keyPair = kpg.generateKeyPair()
    val privateKey = keyPair.getFirst()
    val publicKey = keyPair.getSecond()

    val function = kpg.getPublicKeyGenerationFunction()
    val otherInput: StringElement = StringMonoid.getInstance(Alphabet.UNICODE_BMP).getElement(proverId)

    val challengeGenerator: SigmaChallengeGenerator  = FiatShamirSigmaChallengeGenerator.getInstance(
      Csettings.group.getZModOrder(), otherInput, convertMethod, hashMethod, converter)

    val pg: PlainPreimageProofSystem = PlainPreimageProofSystem.getInstance(challengeGenerator, function)
    val proof: Triple = pg.generate(privateKey, publicKey)

    val success = pg.verify(proof, publicKey)
    if (!success) {
      throw new Exception("Failed verifying proof")
    } else {
      println("createShare: verified share ok")
      // println("Commitment: " + pg.getChallenge(proof) + " challenge:" + pg.getCommitment(proof) + " response: " + pg.getResponse(proof))
      // println("ZKP for shared key: Challenge-Space: " + pg.getChallengeSpace() + "Commitment-Space: " + pg.getCommitmentSpace() + "public Key:" + publicKey)
    }

    val sigmaProofDTO = SigmaProofDTO(pg.getCommitment(proof).convertToString(), pg.getChallenge(proof).convertToString(), pg.getResponse(proof).convertToString())

    (EncryptionKeyShareDTO(sigmaProofDTO, publicKey.convertToBigInteger().toString(10)), privateKey.convertToBigInteger().toString)
  }

  def partialDecrypt(votes: Seq[Tuple], privateKey: BigInteger, proverId: String, Csettings: CryptoSettings) = {

    val encryptionGenerator = Csettings.generator

    val secretKey = Csettings.group.getZModOrder().getElementFrom(privateKey)
    println(s"PartialDecrypt: keymaker using secretKey $secretKey")
    val decryptionKey = secretKey.invert()
    val publicKey = encryptionGenerator.selfApply(secretKey)

    val lists = votes.map { v =>

      val element = v.getFirst()
      if(element.convertToString == "1") {
        // FIXME
        println("********** Crash incoming!")
      }
      // println(s"partial decryption: $element")
      val function: GeneratorFunction = GeneratorFunction.getInstance(element)
      val partialDecryption = function.apply(decryptionKey).asInstanceOf[GStarModElement]

      (partialDecryption, function)
    }.unzip

    val proofDTO = createProof(proverId, secretKey, publicKey, lists._1, lists._2, Csettings)

    PartialDecryptionDTO(lists._1, proofDTO)
  }

  private def createProof(proverId: String, secretKey: Element[_],
      publicKey: Element[_], partialDecryptions: Seq[Element[_]], generatorFunctions: Seq[Function], Csettings: CryptoSettings) = {

    val encryptionGenerator = Csettings.generator

    // Create proof functions
    val f1: Function = GeneratorFunction.getInstance(encryptionGenerator)
    val f2: Function = CompositeFunction.getInstance(
        InvertFunction.getInstance(Csettings.group.getZModOrder()),
        MultiIdentityFunction.getInstance(Csettings.group.getZModOrder(), generatorFunctions.length),
        ProductFunction.getInstance(generatorFunctions :_*))

    // Private and public input and prover id
    val privateInput = secretKey
    val publicInput: Pair = Pair.getInstance(publicKey, Tuple.getInstance(partialDecryptions:_*))
    val otherInput = StringMonoid.getInstance(Alphabet.UNICODE_BMP).getElement(proverId)

    val challengeGenerator: SigmaChallengeGenerator = FiatShamirSigmaChallengeGenerator.getInstance(
        Csettings.group.getZModOrder(), otherInput, convertMethod, hashMethod, converter)
    val proofSystem: EqualityPreimageProofSystem = EqualityPreimageProofSystem.getInstance(challengeGenerator, f1, f2)
    // Generate and verify proof
    val proof: Triple = proofSystem.generate(privateInput, publicInput)
    val result = proofSystem.verify(proof, publicInput)
    // FIXME do something if verify broken
    // println(s"Decryption proof $result")

    SigmaProofDTO(proofSystem.getCommitment(proof).convertToString(), proofSystem.getChallenge(proof).convertToString(), proofSystem.getResponse(proof).convertToString())
  }
}

object Mixer extends ProofSettings {

  def shuffle(ciphertexts: Tuple, publicKey: Element[_], Csettings: CryptoSettings, proverId: String) = {
    import scala.collection.JavaConversions._

    val elGamal = ElGamalEncryptionScheme.getInstance(Csettings.generator)

    println("===== ciphertexts =====")
    println(ciphertexts)
    println("===== ciphertexts =====")

    val mixer: ReEncryptionMixer = ReEncryptionMixer.getInstance(elGamal, publicKey, ciphertexts.getArity())
    val psi: PermutationElement = mixer.getPermutationGroup().getRandomElement()

    val rs: Tuple = mixer.generateRandomizations()

    // Perfom shuffle
    val shuffledVs: Tuple = mixer.shuffle(ciphertexts, psi, rs)

    println("===== shuffled  =====")
    println(shuffledVs)
    println("===== shuffled  =====")

    // Create sigma challenge generator
    val otherInput: StringElement = StringMonoid.getInstance(Alphabet.UNICODE_BMP).getElement(proverId)

    val challengeGenerator: SigmaChallengeGenerator = FiatShamirSigmaChallengeGenerator.getInstance(
        Csettings.group.getZModOrder(), otherInput, convertMethod, hashMethod, converter)

    // Create e-values challenge generator
    val ecg: ChallengeGenerator = PermutationCommitmentProofSystem.createNonInteractiveEValuesGenerator(
        Csettings.group.getZModOrder(), ciphertexts.getArity())

    // val pcs: PermutationCommitmentScheme = PermutationCommitmentScheme.getInstance(group, ciphertexts.getArity())
    val pcs: PermutationCommitmentScheme = PermutationCommitmentScheme.getInstance(Csettings.group, ciphertexts.getArity())
    val permutationCommitmentRandomizations: Tuple = pcs.getRandomizationSpace().getRandomElement()
    val permutationCommitment: Tuple = pcs.commit(psi, permutationCommitmentRandomizations)

    // Create psi commitment proof system
    val pcps: PermutationCommitmentProofSystem = PermutationCommitmentProofSystem.getInstance(challengeGenerator, ecg,
        Csettings.group, ciphertexts.getArity())

    // Create psi commitment proof
    val privateInputPermutation: Pair = Pair.getInstance(psi, permutationCommitmentRandomizations)
    val publicInputPermutation = permutationCommitment
    val permutationProof: Tuple = pcps.generate(privateInputPermutation, publicInputPermutation)

    // 2. Shuffle Proof
    //------------------
    // Create shuffle proof system
    val spg: ReEncryptionShuffleProofSystem = ReEncryptionShuffleProofSystem.getInstance(challengeGenerator, ecg, ciphertexts.getArity(), elGamal, publicKey)

    // Proof and verify
    val privateInputShuffle: Tuple = Tuple.getInstance(psi, permutationCommitmentRandomizations, rs)
    val publicInputShuffle: Tuple = Tuple.getInstance(permutationCommitment, ciphertexts, shuffledVs)

    // Create shuffle proof
    val mixProof: Tuple = spg.generate(privateInputShuffle, publicInputShuffle)

    val bridgingCommitments = pcps.getBridingCommitment(permutationProof).asInstanceOf[Tuple]
    val eValues = pcps.getEValues(permutationProof).asInstanceOf[Tuple]

    val permputationProofDTO = PermutationProofDTO(pcps.getCommitment(permutationProof).convertToString(),
      pcps.getChallenge(permutationProof).convertToString(),
      pcps.getResponse(permutationProof).convertToString(),
      bridgingCommitments.map(x => x.convertToString).toSeq,
      eValues.map(x => x.convertToString).toSeq)

    // println(s"Permutation proof ****\n$permutationProof")

    val eValues2 = spg.getEValues(mixProof).asInstanceOf[Tuple]

    val mixProofDTO = MixProofDTO(spg.getCommitment(mixProof).convertToString(),
      spg.getChallenge(mixProof).convertToString(),
      spg.getResponse(mixProof).convertToString(),
      eValues2.map(x => x.convertToString).toSeq)

    val shuffleProofDTO = ShuffleProofDTO(mixProofDTO, permputationProofDTO, permutationCommitment.convertToString)

    // println(s"Mix proof *****\n$mixProof")
    // println(shuffleProofDTO)

    val pcps2: PermutationCommitmentProofSystem = PermutationCommitmentProofSystem.getInstance(challengeGenerator, ecg,
      Csettings.group, ciphertexts.getArity())
    val spg2: ReEncryptionShuffleProofSystem = ReEncryptionShuffleProofSystem.getInstance(challengeGenerator, ecg, ciphertexts.getArity(), elGamal, publicKey)

    val v1 = pcps2.verify(permutationProof, publicInputPermutation)
    // Verify shuffle proof
    val v2 = spg2.verify(mixProof, publicInputShuffle)
    // Verify equality of permutation commitments
    val v3 =
      publicInputPermutation.isEquivalent(publicInputShuffle.getFirst())

    println("Verification ok: " + (v1 && v2 && v3))

    val votesString = Util.seqFromTuple(shuffledVs).map( x => x.convertToString )
    ShuffleResultDTO(shuffleProofDTO, votesString)
  }
}