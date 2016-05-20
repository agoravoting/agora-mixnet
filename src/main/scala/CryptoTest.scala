package app

import ch.bfh.unicrypt.crypto.keygenerator.interfaces.KeyPairGenerator
import ch.bfh.unicrypt.crypto.schemes.encryption.classes.ElGamalEncryptionScheme
import ch.bfh.unicrypt.math.algebra.general.interfaces.Element
import ch.bfh.unicrypt.math.algebra.multiplicative.classes.GStarModSafePrime
import java.math.BigInteger
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}

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
  // testJnaGmp()

  def testShuffle() = {
    val elGamal = ElGamalEncryptionScheme.getInstance(Csettings.generator)
    val keyPair = elGamal.getKeyPairGenerator().generateKeyPair()
    val privateKey = keyPair.getFirst()
    val publicKey = keyPair.getSecond()
    val votes = Util.getRandomVotes(10, Csettings.generator, publicKey)

    val shuffleResult = MX.shuffle(Util.tupleFromSeq(votes), publicKey, Csettings, "proverId")
    val shuffled = shuffleResult.votes.map( v => Util.getE(elGamal.getEncryptionSpace, v) )

    Verifier.verifyShuffle(Util.tupleFromSeq(votes), Util.tupleFromSeq(shuffled),
      shuffleResult.shuffleProof, "proverId", publicKey, Csettings)

    shuffled.foreach { v =>
      val decryption = elGamal.decrypt(privateKey, v)
      println("decrypted " + decryption)
    }
  }

  def testDkgAndJointDecryption() = {
    implicit val system = ActorSystem()
    implicit val executor = system.dispatchers.lookup("my-other-dispatcher")
    implicit val materializer = ActorMaterializer()
    KM.createShare("1", Csettings) flatMap { case (share, key) => 
      addShare(share, "1", Csettings, key)
      KM.createShare("2", Csettings)
    } map { case (share2, key2) => 
      addShare(share2, "2", Csettings, key2)
      println(s"Shares $shares")
      val publicKey = combineShares(shares, Csettings)
  
      val ciphertexts = Util.getRandomVotes(10, Csettings.generator, publicKey)
  
      // a^-x1
      val elementsOne = KM.partialDecrypt(ciphertexts, privates(0), "0", Csettings)
      var ok = Verifier.verifyPartialDecryption(elementsOne, ciphertexts, Csettings, "0", shares(0))
      if(!ok) throw new Exception()
      // a^-x2
      val elementsTwo = KM.partialDecrypt(ciphertexts, privates(1), "1", Csettings)
      ok = Verifier.verifyPartialDecryption(elementsTwo, ciphertexts, Csettings, "1", shares(1))
      if(!ok) throw new Exception()
  
      println(s"partial decrypts one ****\n$elementsOne")
      println(s"partial decrypts two ****\n $elementsTwo")
      // a^-x = a^-x1 * a^-x2 ...
      val combined = (elementsOne.partialDecryptions.map(Csettings.group.getElementFrom(_))
        zip elementsTwo.partialDecryptions.map(Csettings.group.getElementFrom(_))).map(c => c._1.apply(c._2))
      println(s"a^-x ****\n$combined")
      // a^-x * b = m
      val decrypted = (ciphertexts zip combined).map(c => c._1.getSecond().apply(c._2))
      println(s"Decrypted $decrypted")
    }     
  }

  def testJnaGmp() = {
    import com.squareup.jnagmp._

    val base = new BigInteger("31232")
    val pow = new BigInteger("170141183")
    val mod = new BigInteger("17015")

    val pResult = Gmp.modPowSecure(base, pow, mod)
    println(pResult)

    println(base.modPow(pow, mod))
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