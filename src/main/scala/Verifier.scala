import java.math.BigInteger

import ch.bfh.unicrypt.crypto.proofsystem.challengegenerator.interfaces.ChallengeGenerator
import ch.bfh.unicrypt.crypto.proofsystem.challengegenerator.interfaces.SigmaChallengeGenerator
import ch.bfh.unicrypt.crypto.proofsystem.classes.PermutationCommitmentProofSystem
import ch.bfh.unicrypt.crypto.proofsystem.classes.ReEncryptionShuffleProofSystem
import ch.bfh.unicrypt.crypto.schemes.commitment.classes.PermutationCommitmentScheme
import ch.bfh.unicrypt.crypto.schemes.encryption.classes.ElGamalEncryptionScheme
import ch.bfh.unicrypt.crypto.schemes.encryption.interfaces.ReEncryptionScheme
import ch.bfh.unicrypt.helper.math.Alphabet;
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
import ch.bfh.unicrypt.math.algebra.multiplicative.classes.GStarMod
import ch.bfh.unicrypt.math.algebra.multiplicative.classes.GStarModSafePrime
import ch.bfh.unicrypt.crypto.proofsystem.challengegenerator.classes.FiatShamirSigmaChallengeGenerator;
import ch.bfh.unicrypt.helper.converter.classes.ConvertMethod;
import ch.bfh.unicrypt.helper.converter.classes.biginteger.ByteArrayToBigInteger;
import ch.bfh.unicrypt.helper.converter.classes.bytearray.BigIntegerToByteArray;
import ch.bfh.unicrypt.helper.converter.classes.bytearray.StringToByteArray;
import ch.bfh.unicrypt.helper.converter.interfaces.Converter;
import ch.bfh.unicrypt.helper.hash.HashAlgorithm;
import ch.bfh.unicrypt.helper.hash.HashMethod;
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
import ch.bfh.unicrypt.math.function.classes.CompositeFunction;
import ch.bfh.unicrypt.math.function.classes.GeneratorFunction;
import ch.bfh.unicrypt.math.function.classes.InvertFunction;
import ch.bfh.unicrypt.math.function.classes.MultiIdentityFunction;
import ch.bfh.unicrypt.math.function.classes.ProductFunction;
import ch.bfh.unicrypt.math.function.interfaces.Function;

import java.nio.ByteOrder
import java.nio.charset.Charset

object Verifier {
  val convertMethod = ConvertMethod.getInstance(
        BigIntegerToByteArray.getInstance(ByteOrder.BIG_ENDIAN),
        StringToByteArray.getInstance(Charset.forName("UTF-8")))
  val hashAlgorithm: HashAlgorithm = HashAlgorithm.SHA256
  val converter = ByteArrayToBigInteger.getInstance(hashAlgorithm.getByteLength(), 1)

  def verifyKeyShare(share: EncryptionKeyShareDTO, Csettings: CryptoSettings, proverId: String) = {
    println("Verifier: verifyKeyShare......")
    val elGamal = ElGamalEncryptionScheme.getInstance(Csettings.generator)
    val keyPairGen: KeyPairGenerator = elGamal.getKeyPairGenerator();
    val publicKey = keyPairGen.getPublicKeySpace().getElementFrom(share.keyShare)
    val proofFunction = keyPairGen.getPublicKeyGenerationFunction()

    val otherInput: StringElement = StringMonoid.getInstance(Alphabet.UNICODE_BMP).getElement(proverId)

    val hashMethod = HashMethod.getInstance(hashAlgorithm)

    val challengeGenerator: SigmaChallengeGenerator = FiatShamirSigmaChallengeGenerator.getInstance(
        Csettings.group.getZModOrder(), otherInput, convertMethod, hashMethod, converter)

    val pg: PlainPreimageProofSystem = PlainPreimageProofSystem.getInstance(challengeGenerator, proofFunction)

    //Fill triple
    val commitment
        = pg.getCommitmentSpace().getElementFrom(share.sigmaProofDTO.commitment)
    val challenge
        = pg.getChallengeSpace().getElementFrom(share.sigmaProofDTO.challenge)
    val response
        = pg.getResponseSpace().getElementFrom(share.sigmaProofDTO.response)

    val proofTriple: Triple = Triple.getInstance(commitment, challenge, response);
    println("Commitment: " + commitment + " challenge:" + challenge + " response: " + response);
    println("ZKP for shared key: Challenge-Space: " + pg.getChallengeSpace() + "Commitment-Space: " + pg.getCommitmentSpace() + "public Key:" + publicKey);
    val result = pg.verify(proofTriple, publicKey)
    println(s"Verifier: verifyKeyShare......$result")
    result
  }

  def verifyPartialDecryptions(pd: PartialDecryptionDTO, votes: Seq[Pair], Csettings: CryptoSettings,
      proverId: String, publicKey: Element[_]) = {

    val encryptionGenerator = Csettings.generator
    val generatorFunctions = votes.map { x: Pair =>
      GeneratorFunction.getInstance(x.getFirst)
    }

    // Create proof functions
    val f1: Function = GeneratorFunction.getInstance(encryptionGenerator)
    val f2: Function = CompositeFunction.getInstance(
        InvertFunction.getInstance(Csettings.group.getZModOrder()),
        MultiIdentityFunction.getInstance(Csettings.group.getZModOrder(), generatorFunctions.length),
        ProductFunction.getInstance(generatorFunctions :_*))

    val publicInput: Pair = Pair.getInstance(publicKey, Tuple.getInstance(pd.partialDecryptions:_*))
    val otherInput = StringMonoid.getInstance(Alphabet.UNICODE_BMP).getElement(proverId);
    val hashMethod = HashMethod.getInstance(hashAlgorithm);

    val challengeGenerator: SigmaChallengeGenerator = FiatShamirSigmaChallengeGenerator.getInstance(
        Csettings.group.getZModOrder(), otherInput, convertMethod, hashMethod, converter);
    val proofSystem: EqualityPreimageProofSystem = EqualityPreimageProofSystem.getInstance(challengeGenerator, f1, f2);

    val commitment
        = proofSystem.getCommitmentSpace().getElementFrom(pd.proofDTO.commitment)
    val challenge
        = proofSystem.getChallengeSpace().getElementFrom(pd.proofDTO.challenge)
    val response
        = proofSystem.getResponseSpace().getElementFrom(pd.proofDTO.response)

    val proof: Triple = Triple.getInstance(commitment, challenge, response)
    val result = proofSystem.verify(proof, publicInput)
    println(s"Verifier: verifyPartialDecryptions $result")
  }

  def verifyShuffle(votes: Tuple, votesShuffled: Tuple) = {
    /* val pcps2: PermutationCommitmentProofSystem = PermutationCommitmentProofSystem.getInstance(challengeGenerator, ecg,
      group, ciphertexts.getArity())
    val spg2: ReEncryptionShuffleProofSystem = ReEncryptionShuffleProofSystem.getInstance(challengeGenerator, ecg, ciphertexts.getArity(), elGamal, pk);

    val v1 = pcps2.verify(permutationProof, publicInputPermutation)
    // Verify shuffle proof
    val v2 = spg2.verify(mixProof, publicInputShuffle)
    // Verify equality of permutation commitments
    val v3 =
      publicInputPermutation.isEquivalent(publicInputShuffle.getFirst())

    println("Verification ok: " + (v1 && v2 && v3))*/
  }
}