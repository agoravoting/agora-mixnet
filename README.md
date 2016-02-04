ElectionTest demo
=================

Minimal voting demo using [unicrypt](https://github.com/bfh-evg/univote2) plus code/design from [univote](https://github.com/bfh-evg/univote2). Below is the header to the ElectionTest App object found in [Election.scala](https://github.com/agoravoting/sandbox/blob/master/src/main/scala/Election.scala):

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
  
Setting it up
-------------

     git clone https://github.com/agoravoting/sandbox.git

     apt-get install rng-tools

     install sbt - http://www.scala-sbt.org/download.html

Running it
----------

You need to have sbt in your path. Then from the root of the sandbox repo

     sbt run

The first time sbt will have to download all the dependencies and compile the project, it
may take a while.

You can also enter the sbt console and issue commands from there

     sbt
     >

From the prompt you can issue commands like

     > clean
     > compile
     > run
     > assembly

sbt commands auto-complete with the tab key, so you can explore more options. The assembly command produces a jar with all the dependencies included that you run like this, for example

     java -classpath target/scala-2.11/sandbox-assembly-0.1-SNAPSHOT.jar ElectionTest 100

which runs the benchmark with 100 votes. If you wish to attach a profiler (like visualvm), you may need to run this

     java -XX:+StartAttachListener -classpath target/scala-2.11/sandbox-assembly-0.1-SNAPSHOT.jar ElectionTest 3000