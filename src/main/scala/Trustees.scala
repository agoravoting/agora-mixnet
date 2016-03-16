import scala.collection.mutable.{ Map => MutableMap }

import ch.bfh.unicrypt.crypto.mixer.classes.ReEncryptionMixer
import ch.bfh.unicrypt.crypto.proofsystem.challengegenerator.classes.FiatShamirSigmaChallengeGenerator
import ch.bfh.unicrypt.crypto.proofsystem.challengegenerator.interfaces.ChallengeGenerator
import ch.bfh.unicrypt.crypto.proofsystem.challengegenerator.interfaces.SigmaChallengeGenerator
import ch.bfh.unicrypt.crypto.proofsystem.classes.EqualityPreimageProofSystem
import ch.bfh.unicrypt.crypto.proofsystem.classes.PermutationCommitmentProofSystem
import ch.bfh.unicrypt.crypto.proofsystem.classes.PlainPreimageProofSystem
import ch.bfh.unicrypt.crypto.proofsystem.classes.ReEncryptionShuffleProofSystem
import ch.bfh.unicrypt.crypto.schemes.commitment.classes.PermutationCommitmentScheme
import ch.bfh.unicrypt.math.algebra.general.abstracts.AbstractSet
import ch.bfh.unicrypt.crypto.schemes.encryption.classes.ElGamalEncryptionScheme
import ch.bfh.unicrypt.helper.math.Alphabet
import ch.bfh.unicrypt.math.algebra.concatenative.classes.StringElement
import ch.bfh.unicrypt.math.algebra.concatenative.classes.StringMonoid
import ch.bfh.unicrypt.math.algebra.general.classes.Pair
import ch.bfh.unicrypt.math.algebra.general.classes.PermutationElement
import ch.bfh.unicrypt.math.algebra.general.classes.Triple
import ch.bfh.unicrypt.math.algebra.general.classes.Tuple
import ch.bfh.unicrypt.math.algebra.general.interfaces.Element
import ch.bfh.unicrypt.math.algebra.multiplicative.classes.GStarModElement
import ch.bfh.unicrypt.math.function.classes.CompositeFunction
import ch.bfh.unicrypt.math.function.classes.GeneratorFunction
import ch.bfh.unicrypt.math.function.classes.InvertFunction
import ch.bfh.unicrypt.math.function.classes.MultiIdentityFunction
import ch.bfh.unicrypt.math.function.classes.ProductFunction
import ch.bfh.unicrypt.math.function.interfaces.Function
import shapeless.Sized.sizedToRepr

import mpservice.MPBridgeS
import mpservice.MPBridge
import scala.collection.JavaConversions._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

case class PreShuffleData(mixer: ReEncryptionMixer, psi: PermutationElement, elGamal: ElGamalEncryptionScheme,
  challengeGenerator: SigmaChallengeGenerator, ecg: ChallengeGenerator, permutationCommitmentRandomizations: Tuple,
  permutationCommitment: Tuple)

/**
 * Represents a key maker trustee
 *
 * Mixes in the KeyMaker trait (below) as well as managing an identity and private shares
 */
class KeyMakerTrustee(val id: String, privateShares: MutableMap[String, String] = MutableMap()) extends KeyMaker {
  def createKeyShare(e: Election[_, Shares[_]]) = {
    println("KeyMaker creating share..")

    val (encryptionKeyShareDTO, privateKey) = createShare(id, e.state.cSettings)
    privateShares += (e.state.id -> privateKey)
    encryptionKeyShareDTO
  }

  def partialDecryption(e: Election[_, Decryptions[_]]) = {
    val elGamal = ElGamalEncryptionScheme.getInstance(e.state.cSettings.generator)
    val votes = e.state.votes.par.map( v => Util.getE(elGamal.getEncryptionSpace, v).asInstanceOf[Pair]).seq
    val secretKey = e.state.cSettings.group.getZModOrder().getElementFrom(privateShares(e.state.id))

    partialDecrypt(votes, secretKey, id, e.state.cSettings)
  }
}

/**
 * Represents a mixer trustee
 *
 * Simply mixes in the Mixer trait (below) as well as managing an identity (which is used as the proverId)
 */
class MixerTrustee(val id: String) extends Mixer {
  def shuffleVotes(e: Election[_, Mixing[_]]) = {
    println("Mixer shuffle..")

    val elGamal = ElGamalEncryptionScheme.getInstance(e.state.cSettings.generator)
    val keyPairGen = elGamal.getKeyPairGenerator()
    val publicKey = keyPairGen.getPublicKeySpace().getElementFrom(e.state.publicKey)

    println("Convert votes..")

    MPBridge.a()
    val votes = e.state match {
      case s: Mixing[_0] => e.state.votes.par.map( v => Util.getE(elGamal.getEncryptionSpace, v) ).seq
      case _ => e.state.mixes.toList.last.votes.par.map( v => Util.getE(elGamal.getEncryptionSpace, v) ).seq
    }
    MPBridge.b()

    println("Mixer creating shuffle..")

    shuffle(Util.tupleFromSeq(votes), publicKey, e.state.cSettings, id)
  }

  def preShuffleVotes(e: Election[_, VotesStopped]) = {
    val elGamal = ElGamalEncryptionScheme.getInstance(e.state.cSettings.generator)
    val keyPairGen = elGamal.getKeyPairGenerator()
    val publicKey = keyPairGen.getPublicKeySpace().getElementFrom(e.state.publicKey)

    preShuffle(e.state.votes.size, publicKey, e.state.cSettings, id)
  }

  def shuffleVotes(e: Election[_, Mixing[_]], preData: PreShuffleData, pdtoFuture: Future[PermutationProofDTO]) = {
    println("Mixer..")
    val elGamal = ElGamalEncryptionScheme.getInstance(e.state.cSettings.generator)
    val keyPairGen = elGamal.getKeyPairGenerator()
    val publicKey = keyPairGen.getPublicKeySpace().getElementFrom(e.state.publicKey)
    println("Convert votes..")

    MPBridge.a()
    val votes = e.state match {
      case s: Mixing[_0] => e.state.votes.par.map( v => Util.getE(elGamal.getEncryptionSpace, v) ).seq
      case _ => e.state.mixes.toList.last.votes.par.map( v => Util.getE(elGamal.getEncryptionSpace, v) ).seq
    }
    MPBridge.b()

    println("Mixer creating shuffle..")

    shuffle(Util.tupleFromSeq(votes), publicKey, e.state.cSettings, id, preData, pdtoFuture)
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

    // we return the share dto and the generated private key
    (EncryptionKeyShareDTO(sigmaProofDTO, publicKey.convertToBigInteger().toString), privateKey.convertToBigInteger().toString)
  }

  def partialDecrypt(votes: Seq[Tuple], privateKey: Element[_], proverId: String, Csettings: CryptoSettings) = {

    val encryptionGenerator = Csettings.generator

    val secretKey = Csettings.group.getZModOrder().getElementFrom(privateKey.convertToBigInteger)
    println(s"PartialDecrypt: keymaker using secretKey $secretKey")
    val decryptionKey = secretKey.invert()
    val publicKey = encryptionGenerator.selfApply(secretKey)

    val generators = votes.par.map { v =>
      val element = v.getFirst()
      // ask Rolf about this
      if(element.convertToString == "1") {
        println("********** Crash incoming!")
      }

      GeneratorFunction.getInstance(element)
    }.seq
    val lists = MPBridgeS.ex(generators.map{ generator =>
      val partialDecryption = generator.apply(decryptionKey).asInstanceOf[GStarModElement]
      (partialDecryption, generator)
    }, "2").unzip

    val proofDTO = createProof(proverId, secretKey, publicKey, lists._1, lists._2, Csettings)

    PartialDecryptionDTO(lists._1.par.map(_.convertToString).seq, proofDTO)
  }

  private def createProof(proverId: String, secretKey: Element[_],
      publicKey: Element[_], partialDecryptions: Seq[Element[_]], generatorFunctions: Seq[Function], Csettings: CryptoSettings) = {

    MPBridge.a()
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
    MPBridge.b()

    // Generate and verify proof
    val proof: Triple = proofSystem.generate(privateInput, publicInput)

    //
    // Not doing self verification, enough to do it at the BB
    //
    // val result = proofSystem.verify(proof, publicInput)
    // if(!result) throw new Exception
    //

    SigmaProofDTO(proofSystem.getCommitment(proof).convertToString(), proofSystem.getChallenge(proof).convertToString(), proofSystem.getResponse(proof).convertToString())
  }
}

/**
 * Functions needed for a mixer trustee
 *
 * Creation of shuffles and proofs (Terelius Wikstrom according to Locher-Haenni pdf)
 */
trait Mixer extends ProofSettings {

  // corresponds to the offline phase of the proof of shuffle (permutation for known number of votes)
  def preShuffle(voteCount: Int, publicKey: Element[_], Csettings: CryptoSettings, proverId: String) = {

    val elGamal = ElGamalEncryptionScheme.getInstance(Csettings.generator)

    val mixer: ReEncryptionMixer = ReEncryptionMixer.getInstance(elGamal, publicKey, voteCount)
    val psi: PermutationElement = mixer.getPermutationGroup().getRandomElement()

    val pcs: PermutationCommitmentScheme = PermutationCommitmentScheme.getInstance(Csettings.group, voteCount)
    val permutationCommitmentRandomizations: Tuple = pcs.getRandomizationSpace().getRandomElement()

    val permutationCommitment: Tuple = pcs.commit(psi, permutationCommitmentRandomizations)

    println("Mixer: generators..")

    // Create sigma challenge generator
    val otherInput: StringElement = StringMonoid.getInstance(Alphabet.UNICODE_BMP).getElement(proverId)
    val challengeGenerator: SigmaChallengeGenerator = FiatShamirSigmaChallengeGenerator.getInstance(
        Csettings.group.getZModOrder(), otherInput, convertMethod, hashMethod, converter)

    // Create e-values challenge generator
    val ecg: ChallengeGenerator = PermutationCommitmentProofSystem.createNonInteractiveEValuesGenerator(
        Csettings.group.getZModOrder(), voteCount)

    println("Mixer: permutation proof..")

    val pcps: PermutationCommitmentProofSystem = PermutationCommitmentProofSystem.getInstance(challengeGenerator, ecg,
        Csettings.group, voteCount)

    val privateInputPermutation: Pair = Pair.getInstance(psi, permutationCommitmentRandomizations)
    val publicInputPermutation = permutationCommitment

    // Create psi commitment proof system
    println("Mixer: permutation proof, generating..")

    val permutationProofFuture = Future {
      pcps.generate(privateInputPermutation, publicInputPermutation)
    }.map { permutationProof =>

      val bridgingCommitments = pcps.getBridingCommitment(permutationProof).asInstanceOf[Tuple]
      val eValues = pcps.getEValues(permutationProof).asInstanceOf[Tuple]
      val permutationProofDTO = PermutationProofDTO(pcps.getCommitment(permutationProof).convertToString(),
        pcps.getChallenge(permutationProof).convertToString(),
        pcps.getResponse(permutationProof).convertToString(),
        bridgingCommitments.par.map(x => x.convertToString).seq.toSeq,
        eValues.par.map(x => x.convertToString).seq.toSeq)

      permutationProofDTO
    }

    val preShuffleData = PreShuffleData(mixer, psi, elGamal, challengeGenerator, ecg, permutationCommitmentRandomizations, permutationCommitment)

    (preShuffleData, permutationProofFuture)
  }

  def shuffle(ciphertexts: Tuple, publicKey: Element[_], Csettings: CryptoSettings, proverId: String, pre: PreShuffleData, pdtoFuture: Future[PermutationProofDTO]) = {

    println("Mixer: randomizations..")

    val rs: Tuple = pre.mixer.generateRandomizations()

    println("Mixer: shuffle..")

    // Perfom shuffle
    val shuffledVs: Tuple = pre.mixer.shuffle(ciphertexts, pre.psi, rs)

    println("Mixer: shuffle proof..")
    // 2. Shuffle Proof
    //------------------
    // Create shuffle proof system
    val spg: ReEncryptionShuffleProofSystem = ReEncryptionShuffleProofSystem.getInstance(pre.challengeGenerator, pre.ecg, ciphertexts.getArity(), pre.elGamal, publicKey)

    // Proof and verify
    val privateInputShuffle: Tuple = Tuple.getInstance(pre.psi, pre.permutationCommitmentRandomizations, rs)
    val publicInputShuffle: Tuple = Tuple.getInstance(pre.permutationCommitment, ciphertexts, shuffledVs)

    println("Mixer: shuffle proof, generating..")

    // Create shuffle proof
    val mixProof: Tuple = spg.generate(privateInputShuffle, publicInputShuffle)
    val eValues2 = spg.getEValues(mixProof).asInstanceOf[Tuple]

    // FIXME conversion bug code
    // val commitment = spg.getCommitment(mixProof).convertToString
    // println(s"*** commitment $commitment")
    // spg.getCommitmentSpace.asInstanceOf[AbstractSet[_,_]].getElementFrom(commitment)

    // FIXME whether or not using parallel collection on eValues2.map here is good
    val mixProofDTO = MixProofDTO(spg.getCommitment(mixProof).convertToString,
      spg.getChallenge(mixProof).convertToString,
      spg.getResponse(mixProof).convertToString,
      eValues2.map(x => x.convertToString).toSeq)

    pdtoFuture.map { permutationProofDTO =>
      val shuffleProofDTO = ShuffleProofDTO(mixProofDTO, permutationProofDTO, pre.permutationCommitment.convertToString)

      //
      // Not doing self verification, enough to do it at the BB
      //
      /*
          println("Mixer: verifying..")

          val v1 = pcps.verify(permutationProof, publicInputPermutation)
          MPBridge.z(); MPBridge.y();
          // Verify shuffle proof
          val v2 = spg.verify(mixProof, publicInputShuffle)
          MPBridge.z(); MPBridge.y();
          // Verify equality of permutation commitments
          val v3 = publicInputPermutation.isEquivalent(publicInputShuffle.getFirst())

          println("Verification ok: " + (v1 && v2 && v3))
          if(!(v1 && v2 && v3)) throw new Exception();
      */

      val votesString: Seq[String] = Util.seqFromTuple(shuffledVs).par.map( x => x.convertToString ).seq

      ShuffleResultDTO(shuffleProofDTO, votesString)
    }
  }

  def shuffle(ciphertexts: Tuple, publicKey: Element[_], Csettings: CryptoSettings, proverId: String) = {
    import scala.collection.JavaConversions._
    val elGamal = ElGamalEncryptionScheme.getInstance(Csettings.generator)

    val mixer: ReEncryptionMixer = ReEncryptionMixer.getInstance(elGamal, publicKey, ciphertexts.getArity())
    val psi: PermutationElement = mixer.getPermutationGroup().getRandomElement()

    val pcs: PermutationCommitmentScheme = PermutationCommitmentScheme.getInstance(Csettings.group, ciphertexts.getArity())
    val permutationCommitmentRandomizations: Tuple = pcs.getRandomizationSpace().getRandomElement()

    val permutationCommitment: Tuple = pcs.commit(psi, permutationCommitmentRandomizations)

    println("Mixer: generators..")

    // Create sigma challenge generator
    val otherInput: StringElement = StringMonoid.getInstance(Alphabet.UNICODE_BMP).getElement(proverId)
    val challengeGenerator: SigmaChallengeGenerator = FiatShamirSigmaChallengeGenerator.getInstance(
        Csettings.group.getZModOrder(), otherInput, convertMethod, hashMethod, converter)

    // Create e-values challenge generator
    val ecg: ChallengeGenerator = PermutationCommitmentProofSystem.createNonInteractiveEValuesGenerator(
        Csettings.group.getZModOrder(), ciphertexts.getArity())

    val pcps: PermutationCommitmentProofSystem = PermutationCommitmentProofSystem.getInstance(challengeGenerator, ecg,
        Csettings.group, ciphertexts.getArity())

    // Create psi commitment proof
    val privateInputPermutation: Pair = Pair.getInstance(psi, permutationCommitmentRandomizations)
    val publicInputPermutation = permutationCommitment

    // Create psi commitment proof system
    println("Mixer: permutation proof, generating..")

    val permutationProofFuture = Future {
      pcps.generate(privateInputPermutation, publicInputPermutation)
    }.map { permutationProof =>

      val bridgingCommitments = pcps.getBridingCommitment(permutationProof).asInstanceOf[Tuple].toList
      val eValues = pcps.getEValues(permutationProof).asInstanceOf[Tuple]
      val permutationProofDTO = PermutationProofDTO(pcps.getCommitment(permutationProof).convertToString(),
        pcps.getChallenge(permutationProof).convertToString(),
        pcps.getResponse(permutationProof).convertToString(),
        bridgingCommitments.par.map(x => x.convertToString).seq.toSeq,
        eValues.par.map(x => x.convertToString).seq.toSeq)

      permutationProofDTO
    }

    println("Mixer: randomizations..")

    val rs: Tuple = mixer.generateRandomizations()

    println("Mixer: shuffle..")

    // Perfom shuffle
    val shuffledVs: Tuple = mixer.shuffle(ciphertexts, psi, rs)

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
    val eValues2 = spg.getEValues(mixProof).asInstanceOf[Tuple]

    // FIXME whether or not using parallel collection on eValues2.map here is good
    val mixProofDTO = MixProofDTO(spg.getCommitment(mixProof).convertToString(),
      spg.getChallenge(mixProof).convertToString(),
      spg.getResponse(mixProof).convertToString(),
      eValues2.map(x => x.convertToString).toSeq)

    val permutationProofDTO = Await.result(permutationProofFuture, Duration.Inf)

    val shuffleProofDTO = ShuffleProofDTO(mixProofDTO, permutationProofDTO, permutationCommitment.convertToString)

    //
    // Not doing self verification, enough to do it at the BB
    //
    /*
        println("Mixer: verifying..")

        val v1 = pcps.verify(permutationProof, publicInputPermutation)
        MPBridge.z(); MPBridge.y();
        // Verify shuffle proof
        val v2 = spg.verify(mixProof, publicInputShuffle)
        MPBridge.z(); MPBridge.y();
        // Verify equality of permutation commitments
        val v3 = publicInputPermutation.isEquivalent(publicInputShuffle.getFirst())

        println("Verification ok: " + (v1 && v2 && v3))
        if(!(v1 && v2 && v3)) throw new Exception();
    */

    val votesString: Seq[String] = shuffledVs.par.map( x => x.convertToString ).seq.toList

    ShuffleResultDTO(shuffleProofDTO, votesString)
  }
}