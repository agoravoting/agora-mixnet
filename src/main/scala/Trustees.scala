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
import java.math.BigInteger
import scala.collection.mutable.{Map => MutableMap}

/**
 * Represents a key maker trustee
 *
 * Simply mixes in the KeyMaker trait (below) as well as managing an identity and private shares
 */
class KeyMakerTrustee(val id: String, privateShares: MutableMap[String, String] = MutableMap()) extends KeyMaker {
  def createKeyShare(e: Election[_, Shares[_]]) = {
    println("KeyMaker creating share..")
    // val (encryptionKeyShareDTO, privateKey) = KeyMaker.createShare(id, e.state.cSettings)
    val (encryptionKeyShareDTO, privateKey) = createShare(id, e.state.cSettings)
    privateShares += (e.state.id -> privateKey)
    encryptionKeyShareDTO
  }

  def partialDecryption(e: Election[_, Decryptions[_]]) = {
    val elGamal = ElGamalEncryptionScheme.getInstance(e.state.cSettings.generator)
    val votes = e.state.votes.map( v => elGamal.getEncryptionSpace.getElementFrom(v))
    val secretKey = e.state.cSettings.group.getZModOrder().getElementFrom(privateShares(e.state.id))

    partialDecrypt(votes, secretKey, id, e.state.cSettings)
    // KeyMaker.partialDecrypt(votes, secretKey, id, e.state.cSettings)
  }
}

/**
 * Represents a mixer trustee
 *
 * Simply mixes in the Mixer trait (below) as well as managing an identity
 */
class MixerTrustee(val id: String) extends Mixer {
  def shuffleVotes(e: Election[_, Mixing[_]]) = {
    println("Mixer..")
    val elGamal = ElGamalEncryptionScheme.getInstance(e.state.cSettings.generator)
    val keyPairGen = elGamal.getKeyPairGenerator()
    val publicKey = keyPairGen.getPublicKeySpace().getElementFrom(e.state.publicKey)
    println("Convert votes..")
    val votes = e.state match {
      case s: Mixing[_0] => e.state.votes.map( v => elGamal.getEncryptionSpace.getElementFrom(v) )
      case _ => e.state.mixes.toList.last.votes.map( v => elGamal.getEncryptionSpace.getElementFrom(v) )
    }
    println("Mixer creating shuffle..")
    // Mixer.shuffle(Util.tupleFromSeq(votes), publicKey, e.state.cSettings, id)
    shuffle(Util.tupleFromSeq(votes), publicKey, e.state.cSettings, id)
  }
}

/**
 * Functions needed for a keymaker trustee
 *
 * Creation of key shares and partial decryptions, along with necessary proofs and verification
 */
trait KeyMaker extends ProofSettings {

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
    }

    val sigmaProofDTO = SigmaProofDTO(pg.getCommitment(proof).convertToString(), pg.getChallenge(proof).convertToString(), pg.getResponse(proof).convertToString())

    (EncryptionKeyShareDTO(sigmaProofDTO, publicKey.convertToBigInteger().toString(10)), privateKey.convertToBigInteger().toString)
  }

  def partialDecrypt(votes: Seq[Tuple], privateKey: Element[_], proverId: String, Csettings: CryptoSettings) = {

    val encryptionGenerator = Csettings.generator

    val secretKey = Csettings.group.getZModOrder().getElementFrom(privateKey.convertToBigInteger)
    println(s"PartialDecrypt: keymaker using secretKey $secretKey")
    val decryptionKey = secretKey.invert()
    val publicKey = encryptionGenerator.selfApply(secretKey)

    val lists = votes.map { v =>

      val element = v.getFirst()
      if(element.convertToString == "1") {
        // FIXME
        println("********** Crash incoming!")
      }
      val function: GeneratorFunction = GeneratorFunction.getInstance(element)
      val partialDecryption = function.apply(decryptionKey).asInstanceOf[GStarModElement]

      (partialDecryption, function)
    }.unzip

    val proofDTO = createProof(proverId, secretKey, publicKey, lists._1, lists._2, Csettings)
    // FIXME missing verification of own proof

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
    if(!result) throw new Exception

    SigmaProofDTO(proofSystem.getCommitment(proof).convertToString(), proofSystem.getChallenge(proof).convertToString(), proofSystem.getResponse(proof).convertToString())
  }
}

/**
 * Functions needed for a mixer trustee
 *
 * Creation of shuffles and proofs (Terelius Wikstrom according to Locher-Haenni pdf)
 */
trait Mixer extends ProofSettings {

  def shuffle(ciphertexts: Tuple, publicKey: Element[_], Csettings: CryptoSettings, proverId: String) = {
    import scala.collection.JavaConversions._

    val elGamal = ElGamalEncryptionScheme.getInstance(Csettings.generator)

    // println("===== ciphertexts =====")
    // println(ciphertexts)
    // println("===== ciphertexts =====")

    val mixer: ReEncryptionMixer = ReEncryptionMixer.getInstance(elGamal, publicKey, ciphertexts.getArity())
    val psi: PermutationElement = mixer.getPermutationGroup().getRandomElement()

    println("Mixer: randomizations..")
    val rs: Tuple = mixer.generateRandomizations()

    println("Mixer: shuffle..")
    // Perfom shuffle
    val shuffledVs: Tuple = mixer.shuffle(ciphertexts, psi, rs)

    // println("===== shuffled  =====")
    // println(shuffledVs)
    // println("===== shuffled  =====")

    println("Mixer: generators..")
    // Create sigma challenge generator
    val otherInput: StringElement = StringMonoid.getInstance(Alphabet.UNICODE_BMP).getElement(proverId)

    val challengeGenerator: SigmaChallengeGenerator = FiatShamirSigmaChallengeGenerator.getInstance(
        Csettings.group.getZModOrder(), otherInput, convertMethod, hashMethod, converter)

    // Create e-values challenge generator
    val ecg: ChallengeGenerator = PermutationCommitmentProofSystem.createNonInteractiveEValuesGenerator(
        Csettings.group.getZModOrder(), ciphertexts.getArity())

    println("Mixer: permutation proof..")
    val pcs: PermutationCommitmentScheme = PermutationCommitmentScheme.getInstance(Csettings.group, ciphertexts.getArity())
    val permutationCommitmentRandomizations: Tuple = pcs.getRandomizationSpace().getRandomElement()
    val permutationCommitment: Tuple = pcs.commit(psi, permutationCommitmentRandomizations)

    // Create psi commitment proof system
    val pcps: PermutationCommitmentProofSystem = PermutationCommitmentProofSystem.getInstance(challengeGenerator, ecg,
        Csettings.group, ciphertexts.getArity())

    // Create psi commitment proof
    val privateInputPermutation: Pair = Pair.getInstance(psi, permutationCommitmentRandomizations)
    val publicInputPermutation = permutationCommitment
    println("Mixer: permutation proof, generating..")
    val permutationProof: Tuple = pcps.generate(privateInputPermutation, publicInputPermutation)

    println("Mixer: shuffle proof..")
    // 2. Shuffle Proof
    //------------------
    // Create shuffle proof system
    val spg: ReEncryptionShuffleProofSystem = ReEncryptionShuffleProofSystem.getInstance(challengeGenerator, ecg, ciphertexts.getArity(), elGamal, publicKey)

    // Proof and verify
    val privateInputShuffle: Tuple = Tuple.getInstance(psi, permutationCommitmentRandomizations, rs)
    val publicInputShuffle: Tuple = Tuple.getInstance(permutationCommitment, ciphertexts, shuffledVs)

    println("Mixer: shuffle proof, generating..")
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

    val v1 = pcps.verify(permutationProof, publicInputPermutation)
    // Verify shuffle proof
    val v2 = spg.verify(mixProof, publicInputShuffle)
    // Verify equality of permutation commitments
    val v3 = publicInputPermutation.isEquivalent(publicInputShuffle.getFirst())

    println("Verification ok: " + (v1 && v2 && v3))

    val votesString = Util.seqFromTuple(shuffledVs).map( x => x.convertToString )
    ShuffleResultDTO(shuffleProofDTO, votesString)
  }
}