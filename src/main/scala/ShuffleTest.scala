import java.math.BigInteger

import ch.bfh.unicrypt.crypto.proofsystem.challengegenerator.interfaces.ChallengeGenerator
import ch.bfh.unicrypt.crypto.proofsystem.challengegenerator.interfaces.SigmaChallengeGenerator
import ch.bfh.unicrypt.crypto.proofsystem.classes.PermutationCommitmentProofSystem
import ch.bfh.unicrypt.crypto.proofsystem.classes.ReEncryptionShuffleProofSystem
import ch.bfh.unicrypt.crypto.schemes.commitment.classes.PermutationCommitmentScheme
import ch.bfh.unicrypt.crypto.schemes.encryption.classes.ElGamalEncryptionScheme
import ch.bfh.unicrypt.crypto.schemes.encryption.interfaces.ReEncryptionScheme
import ch.bfh.unicrypt.helper.Alphabet
import ch.bfh.unicrypt.helper.Permutation
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

object ShuffleGenerator extends App {


  shuffleProofGenerator()


  // see also MixAndProofExample.example5
  def shuffleProofGenerator() = {
    // Create cyclic group for random safe prime (20 bits) 1024 takes forever
    val group = GStarModSafePrime.getRandomInstance(20)

    // Create ElGamal encryption scheme and select random public key pk
    val elGamal = ElGamalEncryptionScheme.getInstance(group.getDefaultGenerator())

    // val pk = group.getRandomElement();
    // Create keys
    val keyPair = elGamal.getKeyPairGenerator().generateKeyPair()
    val privateKey = keyPair.getFirst()
    val pk = keyPair.getSecond()

    val n = 10
    var ciphertexts = Tuple.getInstance()

    for(i <- 0 until n) {

      // we are getting random elements from G_q, if we want to encode general elements we need to use an encoder
      // see ElGamalEncryptionExample.example2
      // val encoder = ZModToGStarModSafePrimeEncoder.getInstance(cyclicGroup)

      val element = group.getRandomElement()
      // val element = elGamal.getMessageSpace().getRandomElement()

      System.out.println("plaintext " + element)
      val c = elGamal.encrypt(pk, element)
      // Console.println("ciphertext " + c)
      // val decryption = elGamal.decrypt(privateKey, c)
      // Console.println("decrypted " + decryption)
      ciphertexts = ciphertexts.add(c)
    }

    Console.println("******** CIPHERTEXTS ********")
    Console.println(ciphertexts)

    // Create mixer, a random permutation pi , and randomizations r
    val mixer = ReEncryptionMixer.getInstance(elGamal, pk, n)
    val pi = mixer.getPermutationGroup().getRandomElement()
    val r = mixer.generateRandomizations()
    // Shuffle cipher texts using pi and r
    val shuffledCiphertexts = mixer.shuffle(ciphertexts, pi, r)

    Console.println("******** SHUFFLED ********")
    Console.println(shuffledCiphertexts)

    // Create permutation commitment cpi based on pi and randomizations s
    val pcs =
      PermutationCommitmentScheme.getInstance(group, n)
    val s = pcs.getRandomizationSpace().getRandomElement()
    val cpi = pcs.commit(pi, s)
    // Create permutation commitment proof system
    val pcps =
      PermutationCommitmentProofSystem.getInstance(group, n)
    // Define private and public inputs
    val offlinePrivateInput = Pair.getInstance(pi, s)
    val offlinePublicInput = cpi
    // Generate permutation commitment proof
    val offlineProof =
      pcps.generate(offlinePrivateInput, offlinePublicInput)

    // Create shuffle proof system
    val rsps =
    ReEncryptionShuffleProofSystem.getInstance(group, n, elGamal, pk)
    // Define private and publ icinputs
    val onlinePrivateInput = Triple.getInstance(pi, s, r)
    val onlinePublicInput =
      Triple.getInstance(cpi, ciphertexts, shuffledCiphertexts)
    // Generate shuffle proof
    val onlineProof = rsps.generate(onlinePrivateInput, onlinePublicInput)

    // Verify permutation commitment proof
    // see also MixAndProofExample.example5
    val v1 = pcps.verify(offlineProof , offlinePublicInput)
    // Verify shuffle proof
    val v2 = rsps.verify(onlineProof, onlinePublicInput )
    // Verify equality of permutation commitments
    val v3 =
      offlinePublicInput.isEquivalent(onlinePublicInput.getFirst())
    // if (v1 && v2 && v3) success ( )
    Console.println("Verification ok: " + (v1 && v2 && v3))
    assert(v1 && v2 && v3)

    for(i <- 0 until n) {
      val next = shuffledCiphertexts.getAt(i)
      val decryption = elGamal.decrypt(privateKey, next)
      Console.println("decrypted " + decryption)
    }
  }
}