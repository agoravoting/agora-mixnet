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

/**
 * Minimal tests of crypto for key generation, shuffling and joint decryption
 */
object CryptoTest extends App {

  // val grp = GStarModSafePrime.getInstance(167)
  // val grp = GStarModSafePrime.getInstance(new BigInteger("170141183460469231731687303715884114527"))
  val grp = GStarModSafePrime.getFirstInstance(2048)

  val gen = grp.getDefaultGenerator()
  val Csettings = CryptoSettings(grp, gen)

  val shares = scala.collection.mutable.ArrayBuffer.empty[Element[_]]
  val privates = scala.collection.mutable.ArrayBuffer.empty[Element[_]]

  object KM extends KeyMaker
  object MX extends Mixer

  testDkgAndJointDecryption()
  testShuffle()

  def testShuffle() = {
    val elGamal = ElGamalEncryptionScheme.getInstance(Csettings.generator)
    val keyPair = elGamal.getKeyPairGenerator().generateKeyPair()
    val privateKey = keyPair.getFirst()
    val publicKey = keyPair.getSecond()
    val votes = Util.getRandomVotes(10, Csettings.generator, publicKey)

    val shuffleResult = MX.shuffle(Util.tupleFromSeq(votes), publicKey, Csettings, "proverId")
    val shuffled = shuffleResult.votes.map( v => elGamal.getEncryptionSpace.getElementFrom(v) )

    Verifier.verifyShuffle(Util.tupleFromSeq(votes), Util.tupleFromSeq(shuffled),
      shuffleResult.shuffleProof, "proverId", publicKey, Csettings)

    shuffled.foreach { v =>
      val decryption = elGamal.decrypt(privateKey, v)
      println("decrypted " + decryption)
    }
  }

  def testDkgAndJointDecryption() = {
    val (share, key) = KM.createShare("1", Csettings)
    addShare(share, "1", Csettings, key)
    val (share2, key2) = KM.createShare("2", Csettings)
    addShare(share2, "2", Csettings, key2)
    println(s"Shares $shares")
    val publicKey = combineShares(shares, Csettings)

    val ciphertexts = Util.getRandomVotes(10, Csettings.generator, publicKey)

    // a^-x1
    val elementsOne = KM.partialDecrypt(ciphertexts, privates(0), "0", Csettings)
    var ok = Verifier.verifyPartialDecryptions(elementsOne, ciphertexts, Csettings, "0", shares(0))
    if(!ok) throw new Exception()
    // a^-x2
    val elementsTwo = KM.partialDecrypt(ciphertexts, privates(1), "1", Csettings)
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
      throw new Exception("********** Share failed verification")
    }
  }
}