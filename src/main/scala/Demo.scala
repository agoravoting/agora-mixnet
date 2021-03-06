/**
 * This file is part of agora-mixnet.
 * Copyright (C) 2015-2016  Agora Voting SL <agora@agoravoting.com>

 * agora-mixnet is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License.

 * agora-mixnet is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.

 * You should have received a copy of the GNU Affero General Public License
 * along with agora-mixnet.  If not, see <http://www.gnu.org/licenses/>.
**/

import shapeless._
import nat._
import com.typesafe.config.ConfigFactory
import scala.concurrent._
import scala.concurrent.duration.Duration
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}

import ch.bfh.unicrypt.math.algebra.multiplicative.classes.GStarModSafePrime
import ch.bfh.unicrypt.crypto.schemes.encryption.classes.ElGamalEncryptionScheme
import mpservice._
import app._
import accumulator.BoardReader
import director._
import scala.util.{Success, Failure}
import election._
import models._
import utils.Util

object FiwareDemo extends App {
  
  val config = ConfigFactory.load()
  val useGmp = config.getBoolean("mpservice.use-gmp")
  val useExtractor = config.getBoolean("mpservice.use-extractor")
  MPBridgeS.init(useGmp, useExtractor)
  // actually used in Util.getE
  val bypass = config.getBoolean("bypass-membership-check")
  println(s"* bypass-membership-check: $bypass")
  // actually used in AbstractCyclicGroup constructor
  val generatorsParallel = config.getBoolean("use-generators-parallel")
  println(s"* use-generators-parallel: $generatorsParallel")
  // actually used in Util.getIndependentGenerators constructor
  val generatorsParallelLevel = config.getInt("generators-parallelism-level")
  println(s"* generators-parallelism-level: $generatorsParallelLevel")
  implicit val system = ActorSystem()
  implicit val executor = system.dispatchers.lookup("my-other-dispatcher")
  implicit val materializer = ActorMaterializer()
  
  Election.init()
  
  args.toList.lift(0) match {
    case None =>
      throw new Error("Error, missing input arguments: the first argument should be director or authority")
    case Some(arg1) =>
      arg1 match {
        case "director" =>
          println("director")
          val totalVotes = args.toList.lift(1).getOrElse("5").toInt
          val totalAuthorities = args.toList.lift(2).getOrElse("0").toInt
          val director = totalAuthorities match {
            case 2 =>
              new ElectionDirector[_2](totalVotes)
            case 3 =>
              new ElectionDirector[_3](totalVotes)
            case 4 =>
              new ElectionDirector[_4](totalVotes)
            case 5 =>
              new ElectionDirector[_5](totalVotes)
            case 6 =>
              new ElectionDirector[_6](totalVotes)
            case 7 =>
              new ElectionDirector[_7](totalVotes)
            case 8 =>
              new ElectionDirector[_8](totalVotes)
            case 9 =>
              new ElectionDirector[_9](totalVotes)
            case _ =>
              throw new Error("Error, the number of authorities must be 2 to 9")
          }
          waitAll()
          
        case "authority" =>
          println("authority")
          val totalAuthorities = args.toList.lift(1).getOrElse("0").toInt
          val authorityIndex = args.toList.lift(2).getOrElse("0").toInt
          if( totalAuthorities != 2) {
            throw new Error("Error, the number of authorities can be any number, between 2 and 2")
          }
          if(authorityIndex < 0 && authorityIndex >= totalAuthorities) {
            throw new Error("Error, wrong authority index")
          }
          
          val authority = authorityIndex match {
            case 0 => 
              new ElectionAuthority[_2, _0]()
            case 1 =>
              new ElectionAuthority[_2, _1]()
            case _ =>
              throw new Error("this probably will never happen, but the authority index is not 0 or 1")
          }
          waitAll()
      }
  }
  
  def waitAll() {
    println("Waiting")
    val promise = Promise[Any]()
    Await.ready(promise.future, Duration.Inf)
  }
}

/**
 * An election process DEMO
 *
 * Simulates the steps in the election from public key generation all the way to decryption
 *
 * Things that are included in this demo are:
 *
 * - A typed purely functional data structure modeling the election process and bulletin board (see below)
 *
 * - Cryptography for
 *
 *  a) encrypting votes
 *  b) creating keyshares, proofs and verification
 *  c) shuffling votes, proofs and verification
 *  d) joint (partial) decryption, proofs and verification
 *
 * - Not included
 *
 * Remoting (everything simulated with method calls)
 * Signatures and authentication
 * Error handling
 * Proofs of knowledge of plaintext and verification in vote casting
 *
 *
 * An election is modeled as a typed, purely functional sequential state machine. We use shapeless
 * encoding of natural numbers to provide length-typed lists (aka dependent types), that way we get:
 *
 * 1) The election process logic is captured by types, so illegal transitions
 *    are caught by the compiler and inconsistent states are not possible, for example
 *
 *    It is a compile-time error to try to construct the public key without all the shares
 *    It is a compile-time error to add more shares,shuffles or decryptions than expected
 *    It is a compile-error to start an election with no public key
 *    It is a compile-time error to decrypt without shuffling
 *    etc.
 *
 * 2) Because the election is purely functional, the entire history of the election
 *    can be reconstructed or replayed. A purely functional data structure is in this
 *    sense a general case of an immutable log
 *
 *
 * This demo uses two trustees, ElectionTest3 below shows how number of trustees generalizes
 */

object ElectionTest extends App {
  val config = ConfigFactory.load()
  val useGmp = config.getBoolean("mpservice.use-gmp")
  val useExtractor = config.getBoolean("mpservice.use-extractor")
  MPBridgeS.init(useGmp, useExtractor)
  // actually used in Util.getE
  val bypass = config.getBoolean("bypass-membership-check")
  println(s"* bypass-membership-check: $bypass")
  // actually used in AbstractCyclicGroup constructor
  val generatorsParallel = config.getBoolean("use-generators-parallel")
  println(s"* use-generators-parallel: $generatorsParallel")
  // actually used in Util.getIndependentGenerators constructor
  val generatorsParallelLevel = config.getInt("generators-parallelism-level")
  println(s"* generators-parallelism-level: $generatorsParallelLevel")

  val totalVotes = args.toList.lift(0).getOrElse("100").toInt
  implicit val system = ActorSystem()
  implicit val executor = system.dispatchers.lookup("my-other-dispatcher")
  implicit val materializer = ActorMaterializer()

  Election.init()
  
  // create the keymakers
  // these are responsible for distributed key generation and joint decryption
  val k1 = new KeyMakerTrustee("keymaker one")
  val k2 = new KeyMakerTrustee("keymaker two")

  // create the mixers
  // these are responsible for shuffling the votes
  val m1 = new MixerTrustee("mixer one")
  val m2 = new MixerTrustee("mixer two")
   
  // create the election,
  // we are using privacy level 2, two trustees of each kind
  // we are 2048 bits for the size of the group modulus
  val start = Election.create[_2]("my election", 2048, None)
    
  // get subscriber when we have the uid
  // then subscribe to the election creation
  var uidPromise = Promise[String]()
  BoardReader.addElectionCreationListener { uid =>
    if(!uidPromise.isCompleted) {
      uidPromise.success(uid)
    }
  }
  val createdPromise = Promise[Election[_, Created]]()
  val startSharesPromise = Promise[Election[_, Shares[_0]]]()
  uidPromise.future map { uid =>
    val subscriber = BoardReader.getSubscriber(uid)
    subscriber.create() onComplete {
      case Success(dd) =>
        println("GGG subscriber gives created!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
        createdPromise.success(dd)
      case Failure(e) =>
        println(s"GGG $e")
        createdPromise.failure(e)
    }
    subscriber.startShares() onComplete {
      case Success(dd) =>
        println("GGG subscriber gives startShares!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
        startSharesPromise.success(dd)
      case Failure(e) =>
        println(s"GGG $e")
        startSharesPromise.failure(e)
    }
  }
  // the election is now ready to receive key shares
  val readyForShares = createdPromise.future flatMap { start =>
    Election.startShares(start.asInstanceOf[Election[_2, Created]])
  }

  // each keymaker creates the shares and their proofs, these are added to the election
  val twoShares =  startSharesPromise.future flatMap { readyForShares2 =>
    val readyForShares = readyForShares2.asInstanceOf[Election[_2, Shares[_0]]]
    k1.createKeyShare(readyForShares) flatMap { keyShare =>
      Election.addShare(readyForShares, keyShare, k1.id)
    } flatMap { oneShare =>
      k2.createKeyShare(readyForShares) flatMap { keyShare =>
        Election.addShare(oneShare, keyShare, k2.id)
      }
    }
  }

  // combine the shares from the keymaker trustees, this produces the election public key
  val combined = twoShares flatMap { twoShares => 
    Election.combineShares(twoShares)
  }

  // generate dummy votes
  val plaintexts = Seq.fill(totalVotes)(scala.util.Random.nextInt(1000))
  
  val electionGettingVotes = combined flatMap { combined => 
    val startVotes = Election.startVotes(combined)
    
    // since we are storing information in election as if it were a bulletin board, all
    // the data is stored in a wire-compatible format, that is strings/jsons whatever
    // we reconstruct the public key as if it had been read from such a format
    val publicKey = Util.getPublicKeyFromString(combined.state.publicKey, combined.state.cSettings.generator)
    // encrypt the votes with the public key of the election
    val votes = Util.encryptVotes(plaintexts, combined.state.cSettings, publicKey)
    
    startVotes flatMap { startVotes => 
      // doing this in one step to avoid memory explosion
      Election.addVotes(startVotes, votes.map(_.convertToString).toList)
    }
  }

  // we are only timing the mixing phase
  var mixingStart = System.currentTimeMillis()

  // stop the voting period
  val stopVotes = electionGettingVotes flatMap { electionGettingVotes =>
    // FIXME remove this
    MPBridge.total = 0;
    mixingStart = System.currentTimeMillis()
    Election.stopVotes(electionGettingVotes)
  }
  
  val mixing = stopVotes flatMap { stopVotes => 
    // we can start preshuffling for both mixers
    // that way the second preshuffle will be concurrent with the first mix
    val (predata1, proof1) = m1.preShuffleVotes(stopVotes)
    val (predata2, proof2) = m2.preShuffleVotes(stopVotes)
    // prepare for mixing
    val startMix = Election.startMixing(stopVotes)
    startMix flatMap { startMix => 
      val shuffle1 = m1.shuffleVotes(startMix, predata1, proof1)
      
      // we compose futures, first mix then second mix
      shuffle1 flatMap { shuffle =>
        // the proof is verified and the shuffle is then added to the election, advancing its state
        Election.addMix(startMix, shuffle, m1.id)
      } flatMap { mixOne =>
        // each mixing trustee extracts the needed information from the election
        // and performs the shuffle and proofs
        m2.shuffleVotes(mixOne, predata2, proof2) map { shuffle =>
          // the proof is verified and the shuffle is then added to the election, advancing its state
          Election.addMix(mixOne, shuffle, m2.id)
        }
      }
    }
  }

  // once all the mixes are finished we proceed to decryption
  val all = mixing.flatMap {
    mixTwo => mixTwo
  } flatMap { mixTwo =>
    // we are done mixing
    val stopMix = Election.stopMixing(mixTwo)
    
    var mixingEnd = System.currentTimeMillis()
    val startDecryptions = stopMix flatMap { stopMix =>
      mixingEnd = System.currentTimeMillis()
      // start the partial decryptions
      // if we tried to do this before the mixing was completed, the compiler would protest
      Election.startDecryptions(stopMix)
    }
    
    startDecryptions flatMap { startDecryptions =>
      // each keymaker trustee extracts the votes from the last shuffle from the election and
      // uses their private keys to do the partial decryption and create proofs
      val pd1Future = Future { k1.partialDecryption(startDecryptions) }
      val pd2Future = Future { k2.partialDecryption(startDecryptions) }

      // the two decryption futures execute in parallel
      val decryptions = for {
        pd1 <- pd1Future
        pd2 <- pd2Future
      } yield(pd1, pd2)

      decryptions.map { case (pd1, pd2) =>
        val partialOne = Election.addDecryption(startDecryptions, pd1, k1.id)
        val partialTwo = partialOne flatMap { partialOne => 
          Election.addDecryption(partialOne, pd2, k2.id)
        }

        // the partial decryptions are combined, yielding the plaintexts
        val electionDone = partialTwo flatMap { partialTwo => 
          Election.combineDecryptions(partialTwo)
        }
        
        electionDone map { electionDone => 
          // lets check that everything went well
          // println(s"Plaintexts $plaintexts")
          // println(s"Decrypted ${electionDone.state.decrypted}")
          // println("ok: " + (plaintexts.sorted == electionDone.state.decrypted.map(_.toInt).sorted))

          val mixTime = (mixingEnd - mixingStart) / 1000.0
          val totalTime = (System.currentTimeMillis() - mixingStart) / 1000.0

          println("*************************************************************")
          println(s"finished run with votes = $totalVotes")
          println(s"mixTime: $mixTime")
          println(s"totalTime: $totalTime")
          println(s"sec / vote (mix): ${mixTime / totalVotes}")
          println(s"sec / vote: ${totalTime / totalVotes}")
          println(s"total modExps: ${MPBridge.total}")
          println(s"found modExps: ${MPBridge.found}")
          println(s"found modExps %: ${MPBridge.found/MPBridge.total.toDouble}")
          println(s"extracted modExps: ${MPBridge.getExtracted}")
          println(s"extracted modExps %: ${MPBridge.getExtracted/MPBridge.total.toDouble}")
          println(s"modExps / vote: ${MPBridge.total.toFloat / totalVotes}")
          println("*************************************************************")

          MPBridgeS.shutdown
          //Thread.sleep(5000)
          //BoardPoster.closeSystem()
        }        
      }
    }
  }

  all.onFailure { case e =>
    e.printStackTrace
    MPBridgeS.shutdown
  }
}

/**
 * Same as above but with three trustees
 *
 * Note that everything is done the same way except the type parameter _3 and
 * the number of trustee operations
 *
 */
object ElectionTest3 extends App {
  val totalVotes = args.toList.headOption.getOrElse("100").toInt
  implicit val system = ActorSystem()
  implicit val executor = system.dispatchers.lookup("my-other-dispatcher")
  implicit val materializer = ActorMaterializer()

  val k1 = new KeyMakerTrustee("keymaker one")
  val k2 = new KeyMakerTrustee("keymaker two")
  val k3 = new KeyMakerTrustee("keymaker three")

  val m1 = new MixerTrustee("mixer one")
  val m2 = new MixerTrustee("mixer two")
  val m3 = new MixerTrustee("mixer three")

  // privacy level 3, three trustees of each kind, 512 bits for the size of the group modulus
  val start = Election.create[_3]("my election", 512, None)

  val readyForShares = start flatMap {
    start => Election.startShares(start)
  }  
  
  val threeShares = readyForShares flatMap { readyForShares => 
    k1.createKeyShare(readyForShares) flatMap { keyShare =>
      Election.addShare(readyForShares, keyShare, k1.id)
    } flatMap { oneShare =>
      k2.createKeyShare(readyForShares) flatMap { keyShare =>
        Election.addShare(oneShare, keyShare, k2.id)
      }
    } flatMap { twoShares =>
      k3.createKeyShare(readyForShares) flatMap { keyShare =>
        Election.addShare(twoShares, keyShare, k3.id)
      }
    }      
  }

  val combined = threeShares flatMap {
    threeShares => Election.combineShares(threeShares)
  }

  val plaintexts = Seq.fill(totalVotes)(scala.util.Random.nextInt(10))
  
  val electionGettingVotes = combined flatMap { combined =>
    val startVotes = Election.startVotes(combined)
    val publicKey = Util.getPublicKeyFromString(combined.state.publicKey, combined.state.cSettings.generator)

    val votes = Util.encryptVotes(plaintexts, combined.state.cSettings, publicKey)
    var electionGettingVotes = startVotes
    for (i <- 0 until votes.length) {
      electionGettingVotes = electionGettingVotes flatMap { electionGettingVotes =>
        Election.addVote(electionGettingVotes, votes(i).convertToString)
      }
    }
    electionGettingVotes
  }

  val stopVotes = electionGettingVotes flatMap { electionGettingVotes =>
    Election.stopVotes(electionGettingVotes)
  }

  val startMix = stopVotes flatMap { stopVotes => 
    Election.startMixing(stopVotes)
  }
  
  val mixOne = startMix flatMap { startMix => 
    val shuffle1 = m1.shuffleVotes(startMix)
    Election.addMix(startMix, shuffle1, m1.id)
  }
  
  val mixTwo = mixOne flatMap { mixOne =>
    val shuffle2 = m2.shuffleVotes(mixOne)
    Election.addMix(mixOne, shuffle2, m2.id)
  }
  
  val mixThree = mixTwo flatMap { mixTwo =>
    val shuffle3 = m3.shuffleVotes(mixTwo)
    Election.addMix(mixTwo, shuffle3, m3.id)
  }
  
  val stopMix = mixThree flatMap { mixThree =>
    Election.stopMixing(mixThree)
  }

  val startDecryptions = stopMix flatMap { stopMix =>
    Election.startDecryptions(stopMix)
  }
  
  val partialThree = startDecryptions flatMap { startDecryptions =>
    val pd1 = k1.partialDecryption(startDecryptions)
    val pd2 = k2.partialDecryption(startDecryptions)
    val pd3 = k3.partialDecryption(startDecryptions)
    val partialOne = Election.addDecryption(startDecryptions, pd1, k1.id)
    partialOne flatMap { partialOne => 
      val partialTwo = Election.addDecryption(partialOne, pd2, k2.id)
      partialTwo flatMap { partialTwo =>
        Election.addDecryption(partialTwo, pd3, k3.id)
      }
    }
  }

  val electionDone = partialThree flatMap { partialThree => 
    Election.combineDecryptions(partialThree)
  }
  
  electionDone onSuccess { case electionDone => 
    println(s"Plaintexts $plaintexts")
    println(s"Decrypted ${electionDone.state.decrypted}")
    println("ok: " + (plaintexts.sorted == electionDone.state.decrypted.map(_.toInt).sorted))
  }
}

object ElectionTestSerial extends App {

  val totalVotes = args.toList.lift(0).getOrElse("100").toInt
  val config = ConfigFactory.load()
  val useGmp = config.getBoolean("mpservice.use-gmp")
  val useExtractor = config.getBoolean("mpservice.use-extractor")
  implicit val system = ActorSystem()
  implicit val executor = system.dispatchers.lookup("my-other-dispatcher")//system.dispatcher
  implicit val materializer = ActorMaterializer()
  MPBridgeS.init(useGmp, useExtractor)

  // create the keymakers
  // these are responsible for distributed key generation and joint decryption
  val k1 = new KeyMakerTrustee("keymaker one")
  val k2 = new KeyMakerTrustee("keymaker two")

  // create the mixers
  // these are responsible for shuffling the votes
  val m1 = new MixerTrustee("mixer one")
  val m2 = new MixerTrustee("mixer two")

  // create the election
  // we are using privacy level 2, two trustees of each kind
  // we are 2048 bits for the size of the group modulus
  val start = Election.create[_2]("my election", 2048, None)

  // the election is now ready to receive key shares
  val readyForShares = start flatMap { start => 
    Election.startShares(start)
  }

  // each keymaker creates the shares and their proofs, these are added to the election
  val twoShares = readyForShares flatMap { readyForShares =>
    k1.createKeyShare(readyForShares) flatMap { keyShare =>
      Election.addShare(readyForShares, keyShare, k1.id)
    } flatMap { oneShare =>
      k2.createKeyShare(readyForShares) flatMap { keyShare =>
        Election.addShare(oneShare, keyShare, k2.id)
      }
    }
  }

  // combine the shares from the keymaker trustees, this produces the election public key
  val combined = twoShares flatMap { twoShares =>
    Election.combineShares(twoShares)
  }

  // generate dummy votes
  val plaintexts = Seq.fill(totalVotes)(scala.util.Random.nextInt(10))

  val electionGettingVotes = combined flatMap { combined =>
    // since we are storing information in election as if it were a bulletin board, all
    // the data is stored in a wire-compatible format, that is strings/jsons whatever
    // we reconstruct the public key as if it had been read from such a format
    val publicKey =  Util.getPublicKeyFromString(combined.state.publicKey, combined.state.cSettings.generator)

    // encrypt the votes with the public key of the election
    val votes = Util.encryptVotes(plaintexts, combined.state.cSettings, publicKey)
    // open the election period
    val startVotes = Election.startVotes(combined)
    // add the votes to the election
    var electionGettingVotes = startVotes
    for (i <- 0 until votes.length) {
      electionGettingVotes = electionGettingVotes flatMap { electionGettingVotes =>
        Election.addVote(electionGettingVotes, votes(i).convertToString)
      }
    }
    electionGettingVotes
  }

  // we are only timing the mixing phase
  var mixingStart = System.currentTimeMillis()

  // FIXME remove
  MPBridge.total = 0;

  // stop the voting period
  val stopVotes = electionGettingVotes flatMap { electionGettingVotes =>
    mixingStart = System.currentTimeMillis()
    Election.stopVotes(electionGettingVotes)
  }

  // prepare for mixing
  val startMix = stopVotes flatMap { stopVotes =>
    Election.startMixing(stopVotes)
  }


  // the proof is verified and the shuffle is then added to the election, advancing its state
  val mixOne = startMix flatMap { startMix =>
    // each mixing trustee extracts the needed information from the election
    // and performs the shuffle and proofs
    val shuffle1 = m1.shuffleVotes(startMix)
    Election.addMix(startMix, shuffle1, m1.id)
  }

  // again for the second trustee..
  val mixTwo = mixOne flatMap { mixOne => 
    val shuffle2 = m2.shuffleVotes(mixOne)
    Election.addMix(mixOne, shuffle2, m2.id)
  }

  // we are done mixing
  val stopMix = mixTwo flatMap { mixTwo =>
    Election.stopMixing(mixTwo)
  }

  var mixingEnd = System.currentTimeMillis()

  // start the partial decryptions
  val startDecryptions = stopMix flatMap { stopMix =>
    mixingEnd = System.currentTimeMillis()
    Election.startDecryptions(stopMix)
  }

  // the proofs are verified and the partial decryptions are added to the election,
  val partialTwo = startDecryptions flatMap { startDecryptions =>
    // each keymaker trustee extracts the votes from the last shuffle from the election and
    // uses their private keys to do the partial decryption and create proofs
    val pd1 = k1.partialDecryption(startDecryptions)
    val pd2 = k2.partialDecryption(startDecryptions)
    val partialOne = Election.addDecryption(startDecryptions, pd1, k1.id)
    partialOne flatMap { partialOne => 
      Election.addDecryption(partialOne, pd2, k2.id)
    }
  }

  // the partial decryptions are combined, yielding the plaintexts
  val electionDone = partialTwo flatMap { partialTwo => 
    Election.combineDecryptions(partialTwo)
  }
  
  electionDone flatMap { electionDone =>
    // lets check that everything went well
    println(s"Plaintexts $plaintexts")
    println(s"Decrypted ${electionDone.state.decrypted}")
    println("ok: " + (plaintexts.sorted == electionDone.state.decrypted.map(_.toInt).sorted))

    val mixTime = (mixingEnd - mixingStart) / 1000.0
    val totalTime = (System.currentTimeMillis() - mixingStart) / 1000.0

    println("*************************************************************")
    println(s"finished run with votes = $totalVotes")
    println(s"mixTime: $mixTime")
    println(s"totalTime: $totalTime")
    println(s"sec / vote (mix): ${mixTime / totalVotes}")
    println(s"sec / vote: ${totalTime / totalVotes}")
    println(s"total modExps: ${MPBridge.total}")
    println(s"found modExps: ${MPBridge.found}")
    println(s"found modExps %: ${MPBridge.found/MPBridge.total.toDouble}")
    println(s"extracted modExps: ${MPBridge.getExtracted}")
    println(s"extracted modExps %: ${MPBridge.getExtracted/MPBridge.total.toDouble}")
    println(s"modExps / vote: ${MPBridge.total.toFloat / totalVotes}")
    println("*************************************************************")

    mpservice.MPService.shutdown
  }
}

/**************************** other tests ****************************/

object DecryptionTest extends App {
    implicit val system = ActorSystem()
    implicit val executor = system.dispatchers.lookup("my-other-dispatcher")
    implicit val materializer = ActorMaterializer()

  val group = GStarModSafePrime.getFirstInstance(2048)
  val generator = group.getDefaultGenerator()
  val cSettings = CryptoSettings(group, generator)
  val elGamal = ElGamalEncryptionScheme.getInstance(generator)

  object d1 extends KeyMaker
  object d2 extends KeyMaker

  /*val (e1,pk1) = d1.createShare("d1", cSettings)
  val (e2,pk2) = d2.createShare("d2", cSettings)*/
  
  var keyShare1Opt : Option[(EncryptionKeyShareDTO, String)] = None

  val e1k = d1.createShare("d1", cSettings) map { case (e1,pk1) =>
    keyShare1Opt = Some((e1,pk1))
    Util.getPublicKeyFromString(e1.keyShare, cSettings.generator)
  }
  val e2k = d2.createShare("d2", cSettings) map { case (e2,pk2) =>
    Util.getPublicKeyFromString(e2.keyShare, cSettings.generator)
  }

  e1k map { e1k =>
    e2k map { e2k =>
      keyShare1Opt map { case (e1,pk1) =>
        val publicKey = e1k.apply(e2k)
        
        val pk1e = cSettings.group.getZModOrder().getElementFrom(pk1)
      
        val plaintexts = Seq.fill(300)(scala.util.Random.nextInt(10))
        // encrypt the votes with the public key of the election
        val votes = Util.encryptVotes(plaintexts, cSettings, publicKey)
        println("decrypting..")
      
        MPBridge.total = 0;
      
        MPBridge.y()
        val decryption = d1.partialDecrypt(votes, pk1e, "d1", cSettings)
        MPBridge.z()
      
        MPBridge.y()
        val share = elGamal.getMessageSpace.getElementFrom(e1.keyShare)
      
        val ok = Verifier.verifyPartialDecryption(decryption, votes, cSettings, "d1", share)
        MPBridge.z()
      
        MPBridgeS.shutdown
      }
    }
  }
}

object Issue1 extends App with ProofSettings {
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

  val group = GStarModSafePrime.getFirstInstance(2048)
  val generator = group.getDefaultGenerator()
  val cSettings = CryptoSettings(group, generator)
  val elGamal = ElGamalEncryptionScheme.getInstance(generator)

  val keyPair = elGamal.getKeyPairGenerator().generateKeyPair()
  val privateKey = keyPair.getFirst()
  val publicKey = keyPair.getSecond()

  val otherInput: StringElement = StringMonoid.getInstance(Alphabet.UNICODE_BMP).getElement("asdasd")
  val challengeGenerator: SigmaChallengeGenerator = FiatShamirSigmaChallengeGenerator.getInstance(
        cSettings.group.getZModOrder(), otherInput, convertMethod, hashMethod, converter)

  val ecg: ChallengeGenerator = PermutationCommitmentProofSystem.createNonInteractiveEValuesGenerator(
        cSettings.group.getZModOrder(), 400000)

  val spg: ReEncryptionShuffleProofSystem = ReEncryptionShuffleProofSystem.getInstance(challengeGenerator, ecg, 400000, elGamal, publicKey)

  val commitment = scala.io.Source.fromFile("commitment.dat").mkString
  val commitment2 = spg.getCommitmentSpace().asInstanceOf[AbstractSet[_,_]].getElementFrom(commitment)
}

object Issue3 extends App {
  val grp = GStarModSafePrime.getInstance(167)
  // val grp = GStarModSafePrime.getInstance(new BigInteger("170141183460469231731687303715884114527"))
  // val grp = GStarModSafePrime.getFirstInstance(2048)
  val gen = grp.getDefaultGenerator()
  val Csettings = CryptoSettings(grp, gen)
  val elGamal = ElGamalEncryptionScheme.getInstance(Csettings.generator)

  val keyPair = elGamal.getKeyPairGenerator().generateKeyPair()
  val privateKey = keyPair.getFirst()
  val publicKey = keyPair.getSecond()

  // eventually 0 will be used in Z_q
  val votes = Util.encryptVotes(List(0, 1, 2), Csettings, publicKey)
  votes.foreach { v =>
    val first = v.getFirst
    println(first)
    println(v.getFirst.isGenerator)
    val decryption = elGamal.decrypt(privateKey, v)
    println("decrypted " + decryption)
  }
}