import java.nio.ByteOrder
import java.nio.charset.Charset

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
import mpservice.MPBridgeS

/**
 * Proof settings common for proof generators and verifiers
 *
 * We mix in this trait wherever necessary to ensure consistent use of conversions and hashing
 *
 */
trait ProofSettings {
  val convertMethod = ConvertMethod.getInstance(
        BigIntegerToByteArray.getInstance(ByteOrder.BIG_ENDIAN),
        StringToByteArray.getInstance(Charset.forName("UTF-8")))
  val hashAlgorithm: HashAlgorithm = HashAlgorithm.SHA256
  val hashMethod = HashMethod.getInstance(hashAlgorithm)
  val converter = ByteArrayToBigInteger.getInstance(hashAlgorithm.getByteLength(), 1)
}

/**
 * Verification methods for keyshares, shuffles and partial decryptions
 *
 */
object Verifier extends ProofSettings {

  def verifyKeyShare(share: EncryptionKeyShareDTO, Csettings: CryptoSettings, proverId: String) = {
    println("Verifier: verifyKeyShare......")
    val elGamal = ElGamalEncryptionScheme.getInstance(Csettings.generator)
    val keyPairGen: KeyPairGenerator = elGamal.getKeyPairGenerator();
    val publicKey = keyPairGen.getPublicKeySpace().getElementFrom(share.keyShare)
    val proofFunction = keyPairGen.getPublicKeyGenerationFunction()

    val otherInput: StringElement = StringMonoid.getInstance(Alphabet.UNICODE_BMP).getElement(proverId)

    val challengeGenerator: SigmaChallengeGenerator = FiatShamirSigmaChallengeGenerator.getInstance(
        Csettings.group.getZModOrder(), otherInput, convertMethod, hashMethod, converter)

    val pg: PlainPreimageProofSystem = PlainPreimageProofSystem.getInstance(challengeGenerator, proofFunction)

    //Fill triple
    val commitment = pg.getCommitmentSpace().getElementFrom(share.sigmaProofDTO.commitment)
    val challenge = pg.getChallengeSpace().getElementFrom(share.sigmaProofDTO.challenge)
    val response = pg.getResponseSpace().getElementFrom(share.sigmaProofDTO.response)

    val proofTriple: Triple = Triple.getInstance(commitment, challenge, response)
    println("===== Share verification =====")
    val result = pg.verify(proofTriple, publicKey)
    println(s"Verifier: verifyKeyShare......$result")
    println("===== Share verification =====")
    result
  }

  def verifyPartialDecryptions(pd: PartialDecryptionDTO, votes: Seq[Tuple], Csettings: CryptoSettings, proverId: String, publicKey: Element[_]) = {

    val encryptionGenerator = Csettings.generator
    val generatorFunctions = votes.map { x: Tuple =>
      GeneratorFunction.getInstance(x.getFirst)
    }

    // Create proof functions
    val f1: Function = GeneratorFunction.getInstance(encryptionGenerator)
    val f2: Function = CompositeFunction.getInstance(
        InvertFunction.getInstance(Csettings.group.getZModOrder()),
        MultiIdentityFunction.getInstance(Csettings.group.getZModOrder(), generatorFunctions.length),
        ProductFunction.getInstance(generatorFunctions :_*))

    val pdElements = pd.partialDecryptions.map(Csettings.group.getElementFrom(_))
    // val publicInput: Pair = Pair.getInstance(publicKey, Tuple.getInstance(pd.partialDecryptions:_*))
    val publicInput: Pair = Pair.getInstance(publicKey, Tuple.getInstance(pdElements:_*))
    val otherInput = StringMonoid.getInstance(Alphabet.UNICODE_BMP).getElement(proverId)

    val challengeGenerator: SigmaChallengeGenerator = FiatShamirSigmaChallengeGenerator.getInstance(
        Csettings.group.getZModOrder(), otherInput, convertMethod, hashMethod, converter)
    val proofSystem: EqualityPreimageProofSystem = EqualityPreimageProofSystem.getInstance(challengeGenerator, f1, f2)

    val commitment = proofSystem.getCommitmentSpace().getElementFrom(pd.proofDTO.commitment)
    val challenge = proofSystem.getChallengeSpace().getElementFrom(pd.proofDTO.challenge)
    val response = proofSystem.getResponseSpace().getElementFrom(pd.proofDTO.response)

    val proof: Triple = Triple.getInstance(commitment, challenge, response)
    val result = proofSystem.verify(proof, publicInput)

    println(s"Verifier: verifyPartialDecryptions $result")

    result
  }

  def verifyShuffle(votes: Tuple, shuffledVotes: Tuple, shuffleProof: ShuffleProofDTO,
    proverId: String, publicKey: Element[_], Csettings: CryptoSettings) = {

    val elGamal = ElGamalEncryptionScheme.getInstance(Csettings.generator)

    val otherInput: StringElement = StringMonoid.getInstance(Alphabet.UNICODE_BMP).getElement(proverId)
    val challengeGenerator: SigmaChallengeGenerator = FiatShamirSigmaChallengeGenerator.getInstance(
        Csettings.group.getZModOrder(), otherInput, convertMethod, hashMethod, converter)

    println("Getting proof systems..")
    // Create e-values challenge generator
    val ecg: ChallengeGenerator = PermutationCommitmentProofSystem.createNonInteractiveEValuesGenerator(
        Csettings.group.getZModOrder(), votes.getArity())
    val pcps: PermutationCommitmentProofSystem = PermutationCommitmentProofSystem.getInstance(challengeGenerator, ecg,
      Csettings.group, votes.getArity())

    val spg: ReEncryptionShuffleProofSystem = ReEncryptionShuffleProofSystem.getInstance(challengeGenerator, ecg, votes.getArity(), elGamal, publicKey)

    val pcs: PermutationCommitmentScheme = PermutationCommitmentScheme.getInstance(Csettings.group, votes.getArity())

    // val permutationCommitment = pcs.getCommitmentSpace().getElementFromString(shuffleProof.permutationCommitment)
    val permutationCommitment = MPBridgeS.ex(pcs.getCommitmentSpace().getElementFromString(shuffleProof.permutationCommitment), "1")

    println("Getting values..")

    val commitment1 = MPBridgeS.ex(pcps.getCommitmentSpace().getElementFromString(shuffleProof.permutationProof.commitment), "1")

    val challenge1 = pcps.getChallengeSpace().getElementFrom(shuffleProof.permutationProof.challenge)
    val response1 = pcps.getResponseSpace().getElementFromString(shuffleProof.permutationProof.response)

    val commitment2 = spg.getCommitmentSpace().getElementFromString(shuffleProof.mixProof.commitment)
    val challenge2 = spg.getChallengeSpace().getElementFrom(shuffleProof.mixProof.challenge)
    val response2 = spg.getResponseSpace().getElementFromString(shuffleProof.mixProof.response)


    val permutationProofDTO = shuffleProof.permutationProof
    val mixProofDTO = shuffleProof.mixProof

    println("Converting bridging commitments..")
    // Assume bridging commitments: GStarmod
    val bridgingCommitments = MPBridgeS.ex(permutationProofDTO.bridgingCommitments.par.map { x =>
      Csettings.group.getElementFrom(x)
    }, "1").seq

    println("Converting permutation e values..")
    // Assume evalues: ZMod
    val eValues = permutationProofDTO.eValues.par.map { x =>
      Csettings.group.getZModOrder.getElementFrom(x)
    }.seq
    println("Converting shuffle e values..")
    val eValues2 = mixProofDTO.eValues.par.map { x =>
      Csettings.group.getZModOrder.getElementFrom(x)
    }.seq

    println("Getting proof instances..")
    val permutationProof: Tuple = Tuple.getInstance(Util.tupleFromSeq(eValues), Util.tupleFromSeq(bridgingCommitments),
      commitment1, challenge1, response1)
    val mixProof: Tuple = Tuple.getInstance(Util.tupleFromSeq(eValues2), commitment2, challenge2, response2)

    // println("===== Permutation proof =====")
    // println(permutationProof)
    // println("===== Permutation proof =====")
    // println("===== Mix proof =====")
    // println(mixProof)
    // println("===== Mix proof =====")

    println("Getting public inputs..")
    val publicInputShuffle: Tuple = Tuple.getInstance(permutationCommitment, votes, shuffledVotes)
    val publicInputPermutation = permutationCommitment

    println("Verifying..")
    val v1 = pcps.verify(permutationProof, publicInputPermutation)
    // Verify shuffle proof
    val v2 = spg.verify(mixProof, publicInputShuffle)
    // Verify equality of permutation commitments
    val v3 = publicInputPermutation.isEquivalent(publicInputShuffle.getFirst())

    val result = v1 && v2 && v3
    println(s"Verifier: verifyShuffle: $result")

    result
  } 
}