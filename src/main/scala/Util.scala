import ch.bfh.unicrypt.math.algebra.general.abstracts.AbstractSet
import ch.bfh.unicrypt.math.algebra.general.interfaces.Element
import ch.bfh.unicrypt.crypto.schemes.encryption.classes.ElGamalEncryptionScheme
import ch.bfh.unicrypt.math.algebra.general.interfaces.Element
import ch.bfh.unicrypt.math.algebra.general.classes.Pair
import ch.bfh.unicrypt.math.algebra.general.classes.Tuple
import ch.bfh.unicrypt.crypto.encoder.classes.ZModPrimeToGStarModSafePrime
import ch.bfh.unicrypt.math.algebra.general.classes.ProductSet
import com.typesafe.config.ConfigFactory
import scala.collection.JavaConversions._

/**
 * Some utilities
 */
object Util {
  val unsafe = ConfigFactory.load().getBoolean("bypass-membership-check")
  val generatorsParallel = ConfigFactory.load().getBoolean("use-generators-parallel")

  def tupleFromSeq(items: Seq[Element[_]]) = {
    Tuple.getInstance(items:_*)
  }

  def seqFromTuple(tuple: Tuple): Seq[Element[_]] = {
    tuple.par.map{ x => x }.seq.toSeq
  }

  def getRandomVotes(size: Int, generator: Element[_], publicKey: Element[_]) = {
    val elGamal = ElGamalEncryptionScheme.getInstance(generator)

    (1 to size).map { _ =>
      val element = elGamal.getMessageSpace().getRandomElement()
      println(s"* plaintext $element")
      elGamal.encrypt(publicKey, element)
    }
  }

  def encryptVotes(plaintexts: Seq[Int], cSettings: CryptoSettings, publicKey: Element[_]) = {
    val elGamal = ElGamalEncryptionScheme.getInstance(cSettings.generator)
    val encoder = ZModPrimeToGStarModSafePrime.getInstance(cSettings.group)

    plaintexts.par.map { p =>
      val message = encoder.getDomain().getElementFrom(p)
      val encodedMessage = encoder.encode(message)
      elGamal.encrypt(publicKey, encodedMessage)
    }.seq
  }

  def getPublicKeyFromString(publicKey: String, generator: Element[_]) = {
    val elGamal = ElGamalEncryptionScheme.getInstance(generator)
    val keyPairGen = elGamal.getKeyPairGenerator()
    keyPairGen.getPublicKeySpace().getElementFrom(publicKey)
  }

  /* def getE[A <: Element[B],B](set: AbstractSet[A, B], value: String): Element[B] = {
    set.getElementFromString(value, unsafe)

  }

  // we shouldnt have to have this in addition to the above method, but otherwise the compiler gets confused
  def getE(set: ProductSet, value: String): Element[_] = {
    set.getElementFromString(value, unsafe)
  }*/

  // unicrypt compatible, must set -Dbypass-membership-check=false

  def getE[A <: Element[B],B](set: AbstractSet[A, B], value: String): Element[B] = {
    if(unsafe) {
      set.getElementFromString(value, unsafe)
    }
    else {
      set.getElementFrom(value)
    }
  }

  // we shouldnt have to have this in addition to the above method, but otherwise the compiler gets confused
  def getE(set: ProductSet, value: String): Element[_] = {
    if(unsafe) {
      set.getElementFromString(value, unsafe)
    }
    else {
      set.asInstanceOf[AbstractSet[_, _]].getElementFrom(value)
    }
  }
}