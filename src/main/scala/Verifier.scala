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
import ch.bfh.unicrypt.math.algebra.general.abstracts.AbstractSet
import mpservice.MPBridgeS
import mpservice.MPBridge

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

    val commitment = pg.getCommitmentSpace().getElementFrom(share.sigmaProofDTO.commitment)
    val challenge = pg.getChallengeSpace().getElementFrom(share.sigmaProofDTO.challenge)
    val response = pg.getResponseSpace().getElementFrom(share.sigmaProofDTO.response)

    val proofTriple: Triple = Triple.getInstance(commitment, challenge, response)

    val result = pg.verify(proofTriple, publicKey)
    println(s"Verifier: verifyKeyShare......$result")

    result
  }

  def verifyPartialDecryption(pd: PartialDecryptionDTO, votes: Seq[Tuple], Csettings: CryptoSettings, proverId: String, publicKey: Element[_]) = {

    val encryptionGenerator = Csettings.generator
    val generatorFunctions = votes.par.map { x: Tuple =>
      GeneratorFunction.getInstance(x.getFirst)
    }.seq

    // Create proof functions
    val f1: Function = GeneratorFunction.getInstance(encryptionGenerator)
    val f2: Function = CompositeFunction.getInstance(
        InvertFunction.getInstance(Csettings.group.getZModOrder()),
        MultiIdentityFunction.getInstance(Csettings.group.getZModOrder(), generatorFunctions.length),
        ProductFunction.getInstance(generatorFunctions :_*))

    // FIXME use Util.getE
    val pdElements = MPBridgeS.ex(pd.partialDecryptions.map(Csettings.group.asInstanceOf[AbstractSet[_,_]].getElementFrom(_)), "1")

    val publicInput: Pair = Pair.getInstance(publicKey, Tuple.getInstance(pdElements:_*))
    val otherInput = StringMonoid.getInstance(Alphabet.UNICODE_BMP).getElement(proverId)
    val challengeGenerator: SigmaChallengeGenerator = FiatShamirSigmaChallengeGenerator.getInstance(
        Csettings.group.getZModOrder(), otherInput, convertMethod, hashMethod, converter)
    val proofSystem: EqualityPreimageProofSystem = EqualityPreimageProofSystem.getInstance(challengeGenerator, f1, f2)

    val commitment = MPBridgeS.ex(proofSystem.getCommitmentSpace().getElementFrom(pd.proofDTO.commitment), "1")
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

    // val permutationCommitment = MPBridgeS.ex(pcs.getCommitmentSpace().getElementFromString(shuffleProof.permutationCommitment), "1")
    val permutationCommitment = Util.getE(pcs.getCommitmentSpace(), shuffleProof.permutationCommitment)

    println("Getting values..")

    // val commitment1 = MPBridgeS.ex(pcps.getCommitmentSpace().getElementFromString(shuffleProof.permutationProof.commitment), "1")
    val commitment1 = Util.getE(pcps.getCommitmentSpace(), shuffleProof.permutationProof.commitment)
    val challenge1 = pcps.getChallengeSpace.getElementFrom(shuffleProof.permutationProof.challenge)
    val response1 = pcps.getResponseSpace.asInstanceOf[AbstractSet[_,_]].getElementFrom(shuffleProof.permutationProof.response)

    // FIXME remove this, used to investigate serialization bug
    // val writer = new java.io.PrintWriter(new java.io.File("commitment.dat"))
    // writer.write(shuffleProof.mixProof.commitment); writer.close()
    // println(s"deserialize commitment ${shuffleProof.mixProof.commitment}")
    // println(s"commitmentspace ${spg.getCommitmentSpace}")

    // FIXME remove
    // AbstractSet.debug = true;
    val commitment2 = spg.getCommitmentSpace.asInstanceOf[AbstractSet[_,_]].getElementFrom(shuffleProof.mixProof.commitment)
    // AbstractSet.debug = false;

    val challenge2 = spg.getChallengeSpace.getElementFrom(shuffleProof.mixProof.challenge)
    val response2 = spg.getResponseSpace.asInstanceOf[AbstractSet[_,_]].getElementFrom(shuffleProof.mixProof.response)

    val permutationProofDTO = shuffleProof.permutationProof
    val mixProofDTO = shuffleProof.mixProof

    println("Converting bridging commitments..")

    /*val bridgingCommitments = MPBridgeS.ex(permutationProofDTO.bridgingCommitments.map { x =>
      Csettings.group.getElementFromString(x)
    }, "1")*/

    // bridging commitments: GStarmod
    val bridgingCommitments = permutationProofDTO.bridgingCommitments.par.map { x =>
      Util.getE(Csettings.group, x)
    }.seq

    println("Converting permutation e values..")

    // evalues: ZMod
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

    println("Getting public inputs..")
    val publicInputShuffle: Tuple = Tuple.getInstance(permutationCommitment, votes, shuffledVotes)
    val publicInputPermutation = permutationCommitment

    println("Verifying..")
    val v1 = pcps.verify(permutationProof, publicInputPermutation)

    val v2 = spg.verify(mixProof, publicInputShuffle)

    val v3 = publicInputPermutation.isEquivalent(publicInputShuffle.getFirst())

    val result = v1 && v2 && v3
    println(s"Verifier: verifyShuffle: $result")

    result
  }
}