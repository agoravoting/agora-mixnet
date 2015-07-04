import ch.bfh.unicrypt.crypto.proofsystem.challengegenerator.interfaces.ChallengeGenerator;
import ch.bfh.unicrypt.crypto.proofsystem.challengegenerator.interfaces.SigmaChallengeGenerator;
import ch.bfh.unicrypt.crypto.proofsystem.classes.PermutationCommitmentProofSystem;
import ch.bfh.unicrypt.crypto.proofsystem.classes.ReEncryptionShuffleProofSystem;
import ch.bfh.unicrypt.crypto.schemes.commitment.classes.PermutationCommitmentScheme;
import ch.bfh.unicrypt.crypto.schemes.encryption.classes.ElGamalEncryptionScheme;
import ch.bfh.unicrypt.crypto.schemes.encryption.interfaces.ReEncryptionScheme;
import ch.bfh.unicrypt.helper.Alphabet;
import ch.bfh.unicrypt.helper.Permutation;
import ch.bfh.unicrypt.math.algebra.additive.classes.ECZModPrime;
import ch.bfh.unicrypt.math.algebra.concatenative.classes.StringMonoid;
import ch.bfh.unicrypt.math.algebra.dualistic.classes.ZMod;
import ch.bfh.unicrypt.math.algebra.general.classes.Pair;
import ch.bfh.unicrypt.math.algebra.general.classes.PermutationElement;
import ch.bfh.unicrypt.math.algebra.general.classes.PermutationGroup;
import ch.bfh.unicrypt.math.algebra.general.classes.ProductGroup;
import ch.bfh.unicrypt.math.algebra.general.classes.Triple;
import ch.bfh.unicrypt.math.algebra.general.classes.Tuple;
import ch.bfh.unicrypt.math.algebra.general.interfaces.Element;
import ch.bfh.unicrypt.math.algebra.multiplicative.classes.GStarMod;
import ch.bfh.unicrypt.math.algebra.multiplicative.classes.GStarModSafePrime;

import ch.bfh.unicrypt.math.algebra.params.classes.SECECCParamsFp;
import ch.bfh.unicrypt.math.function.classes.PermutationFunction;
import ch.bfh.unicrypt.random.classes.CounterModeRandomByteSequence;
import ch.bfh.unicrypt.random.classes.PseudoRandomOracle;
import ch.bfh.unicrypt.random.classes.ReferenceRandomByteSequence;
import ch.bfh.unicrypt.random.interfaces.RandomByteSequence;
import ch.bfh.unicrypt.random.interfaces.RandomOracle;
import java.math.BigInteger;

import ch.bfh.unicrypt.math.algebra.general.interfaces.CyclicGroup;
import ch.bfh.unicrypt.crypto.mixer.classes.ReEncryptionMixer;
import ch.bfh.unicrypt.math.algebra.multiplicative.classes.GStarModElement;

public class Test {

  public static void main(String[] args) {
    shuffleProofGenerator();
  }

  // see also MixAndProofExample.example5
  public static void shuffleProofGenerator() {
    // Create cyclic group for random safe prime (20 bits)
    CyclicGroup group = GStarModSafePrime.getRandomInstance(20);

    // Create ElGamal encryption scheme and select random public key pk
    ElGamalEncryptionScheme elGamal = ElGamalEncryptionScheme.getInstance(group.getDefaultGenerator());

    // Element pk = group.getRandomElement();
    // Create keys
    Pair keyPair = elGamal.getKeyPairGenerator().generateKeyPair();
    Element privateKey = keyPair.getFirst();
    Element pk = keyPair.getSecond();

    int n = 10;
    Tuple ciphertexts = Tuple.getInstance();
    for(int i = 0; i < n; i++) {

      // we are getting random elements from G_q, if we want to encode general elements we need to use an encoder
      // see ElGamalEncryptionExample.example2
      // Encoder encoder = ZModToGStarModSafePrimeEncoder.getInstance(cyclicGroup);

      Element element = group.getRandomElement();
      // Element element = elGamal.getMessageSpace().getRandomElement();

      System.out.println("plaintext " + element);
      Pair c = elGamal.encrypt(pk, element);
      // System.out.println("ciphertext " + c);
      // Element decryption = elGamal.decrypt(privateKey, c);
      // System.out.println("decrypted " + decryption);
      ciphertexts = ciphertexts.add(c) ;
    }

    System.out.println("******** CIPHERTEXTS ********");
    System.out.println(ciphertexts);

    // Create mixer, a random permutation pi , and randomizations r
    ReEncryptionMixer mixer = ReEncryptionMixer.getInstance(elGamal , pk , n) ;
    PermutationElement pi = mixer.getPermutationGroup().getRandomElement( ) ;
    Tuple r = mixer.generateRandomizations();
    // Shuffle cipher texts using pi and r
    Tuple shuffledCiphertexts = mixer.shuffle(ciphertexts , pi , r);

    System.out.println("******** SHUFFLED ********");
    System.out.println(shuffledCiphertexts);

    // Create permutation commitment cpi based on pi and randomizations s
    PermutationCommitmentScheme pcs =
      PermutationCommitmentScheme.getInstance(group, n);
    Tuple s = pcs.getRandomizationSpace().getRandomElement();
    Tuple cpi = pcs.commit(pi, s);
    // Create permutation commitment proof system
    PermutationCommitmentProofSystem pcps =
      PermutationCommitmentProofSystem.getInstance(group, n);
    // Define private and public inputs
    Pair offlinePrivateInput = Pair.getInstance(pi , s);
    Element offlinePublicInput = cpi;
    // Generate permutation commitment proof
    Pair offlineProof =
      pcps.generate(offlinePrivateInput, offlinePublicInput);

    // Create shuffle proof system
    ReEncryptionShuffleProofSystem rsps =
    ReEncryptionShuffleProofSystem.getInstance(group, n, elGamal, pk);
    // Define private and publ icinputs
    Triple onlinePrivateInput = Triple.getInstance(pi, s, r);
    Triple onlinePublicInput =
      Triple.getInstance(cpi, ciphertexts, shuffledCiphertexts);
    // Generate shuffle proof
    Triple onlineProof = rsps.generate(onlinePrivateInput, onlinePublicInput);

    // Verify permutation commitment proof
    // see also MixAndProofExample.example5
    boolean v1 = pcps.verify(offlineProof , offlinePublicInput);
    // Verify shuffle proof
    boolean v2 = rsps.verify(onlineProof, onlinePublicInput );
    // Verify equality of permutation commitments
    boolean v3 =
      offlinePublicInput.isEquivalent(onlinePublicInput.getFirst());
    // if (v1 && v2 && v3) success ( ) ;
    System.out.println("Verification: " + (v1 && v2 && v3));
    assert(v1 && v2 && v3);

    for(int i = 0; i < n; i++) {
      Element next = shuffledCiphertexts.getAt(i);
      Element decryption = elGamal.decrypt(privateKey, next);
      System.out.println("decrypted " + decryption);
    }
  }
}