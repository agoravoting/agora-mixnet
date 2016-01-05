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

case class EncryptionKeyShareDTO(sigmaProofDTO: SigmaProofDTO, keyShare: String, privateKey: String)
case class SigmaProofDTO(commitment: String, challenge: String, response: String)

object CryptoTest extends App {

  val group = GStarModSafePrime.getInstance(167)
  println(s"group ${group.getModulus()}")
  println(s"modulus ${group.getZModOrder()}")
  val shares = scala.collection.mutable.ArrayBuffer.empty[Element[_]]
  val privates = scala.collection.mutable.ArrayBuffer.empty[Element[_]]

  testDkgAndJointDecryption()
  testShuffle()

  def testDkgAndJointDecryption() = {
    var share = createShare("1")
    addShare(share, "1")
    share = createShare("2")
    addShare(share, "2")
    println(s"Shares $shares")
    // val pks = privates.map { x: Element[_] => group.getDefaultGenerator.selfApply(x.convertToBigInteger) }
    // println(s"Private keys $pks")
    val publicKey = combineShares(shares)
    var ciphertexts = Tuple.getInstance()
    val elGamal = ElGamalEncryptionScheme.getInstance(group.getDefaultGenerator())

    for(i <- 0 until 10) {
      val element = elGamal.getMessageSpace().getRandomElement()
      System.out.println(s"Dkg and joint decrypt: plaintext $element")
      val c = elGamal.encrypt(publicKey, element)
      ciphertexts = ciphertexts.add(c)
    }
    val elementsOne = partialDecrypt(ciphertexts, privates(0).convertToBigInteger, "0")
    val elementsTwo = partialDecrypt(ciphertexts, privates(1).convertToBigInteger, "1")
    println(s"partial decrypts one ****\n$elementsOne")
    println(s"partial decrypts two ****\n $elementsTwo")
    val combined = (elementsOne zip elementsTwo).map(c => c._1.apply(c._2))
    println(s"a^-x ****\n$combined")
    for(i <- 0 until 10) {
      val next = ciphertexts.getAt(i).asInstanceOf[Pair]
      val second = next.getSecond()
      println(s"joint decryption ${second.apply(combined(i))}")
    }
  }

  def combineShares(shares: Seq[Element[_]]) = {
    var encKey = group.getIdentityElement();

    for (keyShare <- shares) {
      encKey = encKey.apply(keyShare);
    }

    println(s"combineShares: public key $encKey")

    encKey
  }

  def addShare(encryptionKeyShare: EncryptionKeyShareDTO, tallier: String) = {

    val elGamal = ElGamalEncryptionScheme.getInstance(group.getDefaultGenerator())
    val keyPairGen: KeyPairGenerator = elGamal.getKeyPairGenerator();
    val publicKey = keyPairGen.getPublicKeySpace().getElementFrom(encryptionKeyShare.keyShare)
    val proofFunction = keyPairGen.getPublicKeyGenerationFunction();

    val otherInput: StringElement = StringMonoid.getInstance(Alphabet.UNICODE_BMP).getElement(tallier)

    val converter = ByteArrayToBigInteger.getInstance(HashAlgorithm.SHA256.getByteLength(), 1);

    val hashMethod = HashMethod.getInstance(HashAlgorithm.SHA256);
    val convertMethod = ConvertMethod.getInstance(
      BigIntegerToByteArray.getInstance(ByteOrder.BIG_ENDIAN),
      StringToByteArray.getInstance(Charset.forName("UTF-8")));

    val challengeGenerator: SigmaChallengeGenerator = FiatShamirSigmaChallengeGenerator.getInstance(
        group.getZModOrder(), otherInput, convertMethod, hashMethod, converter);

    val pg: PlainPreimageProofSystem = PlainPreimageProofSystem.getInstance(challengeGenerator, proofFunction);

    //Fill triple
    val commitment
        = pg.getCommitmentSpace().getElementFrom(encryptionKeyShare.sigmaProofDTO.commitment)
    val challenge
        = pg.getChallengeSpace().getElementFrom(encryptionKeyShare.sigmaProofDTO.challenge)
    val response
        = pg.getResponseSpace().getElementFrom(encryptionKeyShare.sigmaProofDTO.response)

    val proofTriple: Triple = Triple.getInstance(commitment, challenge, response);
    println("Commitment: " + commitment + " challenge:" + challenge + " response: " + response);
    println("ZKP for shared key: Challenge-Space: " + pg.getChallengeSpace() + "Commitment-Space: " + pg.getCommitmentSpace() + "public Key:" + publicKey);
    if (pg.verify(proofTriple, publicKey)) {

      shares += publicKey
      val privateKey = keyPairGen.getPrivateKeySpace().getElementFrom(encryptionKeyShare.privateKey)

      privates += privateKey
      println(s"Share added $publicKey $privateKey")
    }
    else {
      //Remove tallier
      println("Share failed verification")
    }
  }

  def partialDecrypt(votes: Tuple, privateKey: BigInteger, proverId: String) = {
      import scala.collection.JavaConversions._

      val encryptionGenerator = group.getDefaultGenerator()

      val secretKey = group.getZModOrder().getElementFrom(privateKey);
      println(s"PartialDecrypt: secretKey $secretKey, privateKey $privateKey")
      val decryptionKey = secretKey.invert();
      val publicKey = encryptionGenerator.selfApply(secretKey);

      val partialDecryptions = scala.collection.mutable.ArrayBuffer.empty[Element[_]]


      val generatorFunctions = scala.collection.mutable.ArrayBuffer.empty[Function]
      for (v <- votes) {
        val pair = v.asInstanceOf[Pair]

        val element = pair.getFirst()
        // println(s"partial decryption: $element")
        val function: GeneratorFunction = GeneratorFunction.getInstance(element);
        generatorFunctions += function;
        val partialDecryption = function.apply(decryptionKey)
        partialDecryptions.add(partialDecryption)
      }

      val proofDTO = createProof(proverId, secretKey, publicKey, partialDecryptions, generatorFunctions);

      partialDecryptions
  }


  def createShare(tallier: String) = {

    val elGamal = ElGamalEncryptionScheme.getInstance(group.getDefaultGenerator())

    val kpg = elGamal.getKeyPairGenerator()
    val keyPair = kpg.generateKeyPair()
    val privateKey = keyPair.getFirst()
    val publicKey = keyPair.getSecond()

    val function = kpg.getPublicKeyGenerationFunction();
    val otherInput: StringElement = StringMonoid.getInstance(Alphabet.UNICODE_BMP).getElement(tallier);
    val hashMethod = HashMethod.getInstance(HashAlgorithm.SHA256);
    val convertMethod = ConvertMethod.getInstance(
      BigIntegerToByteArray.getInstance(ByteOrder.BIG_ENDIAN),
      StringToByteArray.getInstance(Charset.forName("UTF-8")));

    val converter = ByteArrayToBigInteger.getInstance(HashAlgorithm.SHA256.getByteLength(), 1);

    val challengeGenerator: SigmaChallengeGenerator  = FiatShamirSigmaChallengeGenerator.getInstance(
      group.getZModOrder(), otherInput, convertMethod, hashMethod, converter);

    val pg: PlainPreimageProofSystem = PlainPreimageProofSystem.getInstance(challengeGenerator, function);
    val proof: Triple = pg.generate(privateKey, publicKey);

    val success = pg.verify(proof, publicKey);
    if (!success) {
      throw new Exception("Math for proof system broken.");
    } else {
      println("createShare: SigmaProof ok")
      // println("Commitment: " + pg.getChallenge(proof) + " challenge:" + pg.getCommitment(proof) + " response: " + pg.getResponse(proof));
      // println("ZKP for shared key: Challenge-Space: " + pg.getChallengeSpace() + "Commitment-Space: " + pg.getCommitmentSpace() + "public Key:" + publicKey);
    }

    val sigmaProofDTO = SigmaProofDTO(pg.getCommitment(proof).convertToString(), pg.getChallenge(proof).convertToString(), pg.getResponse(proof).convertToString())
      EncryptionKeyShareDTO(sigmaProofDTO, publicKey.convertToBigInteger().toString(10), privateKey.convertToBigInteger().toString)
  }

  // see also MixAndProofExample.example5
  def testShuffle() = {

    val elGamal = ElGamalEncryptionScheme.getInstance(group.getDefaultGenerator())

    val keyPair = elGamal.getKeyPairGenerator().generateKeyPair()
    val privateKey = keyPair.getFirst()
    val pk = keyPair.getSecond()

    val n = 10
    var ciphertexts = Tuple.getInstance()
    println("testShuffle")
    println("******** PLAINTEXTS ********")
    for(i <- 0 until n) {

      // we are getting random elements from G_q, if we want to encode general elements we need to use an encoder
      // see ElGamalEncryptionExample.example2
      // val encoder = ZModToGStarModSafePrimeEncoder.getInstance(cyclicGroup)

      val element = elGamal.getMessageSpace().getRandomElement()

      System.out.println(element)
      val c = elGamal.encrypt(pk, element)

      ciphertexts = ciphertexts.add(c)
    }

    println("******** CIPHERTEXTS ********")
    println(ciphertexts)

    val mixer: ReEncryptionMixer = ReEncryptionMixer.getInstance(elGamal, pk, ciphertexts.getArity());
    val psi: PermutationElement = mixer.getPermutationGroup().getRandomElement();

    val rs: Tuple = mixer.generateRandomizations();

    // Perfom shuffle
    val shuffledVs: Tuple = mixer.shuffle(ciphertexts, psi, rs);

    // Create sigma challenge generator
    val otherInput: StringElement = StringMonoid.getInstance(Alphabet.UNICODE_BMP).getElement("asdfasdfasdf");
    val hashMethod = HashMethod.getInstance(HashAlgorithm.SHA256);
    val convertMethod = ConvertMethod.getInstance(
        BigIntegerToByteArray.getInstance(ByteOrder.BIG_ENDIAN),
        StringToByteArray.getInstance(Charset.forName("UTF-8")))

    //val converter = ByteArrayToBigInteger.getInstance(HashAlgorithm.SHA256.getBitLength(), 1)
    val converter = ByteArrayToBigInteger.getInstance(HashAlgorithm.SHA256.getByteLength())

    val challengeGenerator: SigmaChallengeGenerator = FiatShamirSigmaChallengeGenerator.getInstance(
        group.getZModOrder(), otherInput, convertMethod, hashMethod, converter)

    // Create e-values challenge generator
    val ecg: ChallengeGenerator = PermutationCommitmentProofSystem.createNonInteractiveEValuesGenerator(
        group.getZModOrder(), ciphertexts.getArity())

    // val pcs: PermutationCommitmentScheme = PermutationCommitmentScheme.getInstance(group, ciphertexts.getArity());
    val pcs: PermutationCommitmentScheme = PermutationCommitmentScheme.getInstance(group, ciphertexts.getArity())
    val permutationCommitmentRandomizations: Tuple = pcs.getRandomizationSpace().getRandomElement()
    val permutationCommitment: Tuple = pcs.commit(psi, permutationCommitmentRandomizations)

    // Create psi commitment proof system
    val pcps: PermutationCommitmentProofSystem = PermutationCommitmentProofSystem.getInstance(challengeGenerator, ecg,
        group, ciphertexts.getArity())

    // Create psi commitment proof
    val privateInputPermutation: Pair = Pair.getInstance(psi, permutationCommitmentRandomizations)
    val publicInputPermutation = permutationCommitment
    val permutationProof: Tuple = pcps.generate(privateInputPermutation, publicInputPermutation)

    // 2. Shuffle Proof
    //------------------
    // Create shuffle proof system
    val spg: ReEncryptionShuffleProofSystem = ReEncryptionShuffleProofSystem.getInstance(challengeGenerator, ecg, ciphertexts.getArity(), elGamal, pk);

    // Proof and verify
    val privateInputShuffle: Tuple = Tuple.getInstance(psi, permutationCommitmentRandomizations, rs);
    val publicInputShuffle: Tuple = Tuple.getInstance(permutationCommitment, ciphertexts, shuffledVs);

    // Create shuffle proof
    val mixProof: Tuple = spg.generate(privateInputShuffle, publicInputShuffle);

    println("challenge *****\n " + pcps.getChallenge(permutationProof).convertToString())
    println("commitment *****\n " + pcps.getCommitment(permutationProof).convertToString())
    println("response *****\n " + pcps.getResponse(permutationProof).convertToString())

    println("bridging commitments *****\n " + pcps.getBridingCommitment(permutationProof))
    println("evalues *****\n " + pcps.getEValues(permutationProof))

    println(s"Permutation proof ****\n $permutationProof")

    println("challenge *****\n " + spg.getChallenge(mixProof).convertToString())
    println("commitment *****\n " + spg.getCommitment(mixProof).convertToString())
    println("response *****\n " + spg.getResponse(mixProof).convertToString())
    println("evalues *****\n " + spg.getEValues(mixProof))

    println(s"Mix proof *****\n $mixProof")

    val pcps2: PermutationCommitmentProofSystem = PermutationCommitmentProofSystem.getInstance(challengeGenerator, ecg,
      group, ciphertexts.getArity())
    val spg2: ReEncryptionShuffleProofSystem = ReEncryptionShuffleProofSystem.getInstance(challengeGenerator, ecg, ciphertexts.getArity(), elGamal, pk);

    val v1 = pcps2.verify(permutationProof, publicInputPermutation)
    // Verify shuffle proof
    val v2 = spg2.verify(mixProof, publicInputShuffle)
    // Verify equality of permutation commitments
    val v3 =
      publicInputPermutation.isEquivalent(publicInputShuffle.getFirst())

    println("Verification ok: " + (v1 && v2 && v3))

    for(i <- 0 until n) {
      val next = shuffledVs.getAt(i)
      val decryption = elGamal.decrypt(privateKey, next)
      println("decrypted " + decryption)
    }
  }

  def createProof(proverId: String, secretKey: Element[_],
      publicKey: Element[_], partialDecryptions: Seq[Element[_]], generatorFunctions: Seq[Function]) {

    val encryptionGenerator = group.getDefaultGenerator()
    val hashAlgorithm: HashAlgorithm = HashAlgorithm.SHA256;

    // Create proof functions
    val f1: Function = GeneratorFunction.getInstance(encryptionGenerator);
    val f2: Function = CompositeFunction.getInstance(
        InvertFunction.getInstance(group.getZModOrder()),
        MultiIdentityFunction.getInstance(group.getZModOrder(), generatorFunctions.length),
        ProductFunction.getInstance(generatorFunctions :_*));

    // Private and public input and prover id
    val privateInput = secretKey;
    val publicInput: Pair = Pair.getInstance(publicKey, Tuple.getInstance(partialDecryptions:_*));
    val otherInput = StringMonoid.getInstance(Alphabet.UNICODE_BMP).getElement(proverId);
    val hashMethod = HashMethod.getInstance(hashAlgorithm);
    val convertMethod = ConvertMethod.getInstance(
        BigIntegerToByteArray.getInstance(ByteOrder.BIG_ENDIAN),
        StringToByteArray.getInstance(Charset.forName("UTF-8")));

    val converter = ByteArrayToBigInteger.getInstance(hashAlgorithm.getByteLength(), 1);

    val challengeGenerator: SigmaChallengeGenerator = FiatShamirSigmaChallengeGenerator.getInstance(
        group.getZModOrder(), otherInput, convertMethod, hashMethod, converter);
    val proofSystem: EqualityPreimageProofSystem = EqualityPreimageProofSystem.getInstance(challengeGenerator, f1, f2);
    // Generate and verify proof
    val proof: Triple = proofSystem.generate(privateInput, publicInput);
    val result = proofSystem.verify(proof, publicInput);

    println(s"Decryption proof $result")

    SigmaProofDTO(proofSystem.getCommitment(proof).convertToString(), proofSystem.getChallenge(proof).convertToString(), proofSystem.getResponse(proof).convertToString());
  }
}