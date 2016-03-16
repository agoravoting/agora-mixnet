Agora mixnet
============

This is a mixnet voting prototype based around [unicrypt](https://github.com/bfh-evg/univote2) and code/design from [univote](https://github.com/bfh-evg/univote2).

### Cryptography

* encrypting votes
* creating keyshares, proofs and verification
* shuffling votes, proofs and verification
* joint (partial) decryption, proofs and verification

### Typed purely functional bulletin board

An election is modeled as a typed, purely functional sequential state machine. We use shapeless
encoding of natural numbers to provide length-typed lists (aka dependent types). The election process logic is captured by types, so illegal transitions are caught by the compiler and inconsistent states are not possible.

### Not included

* Remoting (everything simulated with method calls)
* Signatures and authentication
* Error handling
* Proofs of knowledge of plaintext and verification in vote casting

Setting it up
-------------

     git clone https://github.com/agoravoting/sandbox.git

     apt-get install rng-tools

     install sbt - http://www.scala-sbt.org/download.html

You must have java installed and JAVA_HOME set.

Running it
----------

You need to have sbt in your path. Then from the root of the sandbox repo

     sbt assembly

The first time sbt will have to download all the dependencies and compile the project, it
may take a while. The assembly command produces a jar with all the dependencies included. Once you have that jar copy the shell scripts from src/main/shell to the home of the project.

     cp src/main/shell/*.sh .

To run an election demo with 100 votes

     ./run.sh 100

You can change optimization flags within the run.sh script:

    USE_GMP=true
    USE_EXTRACTOR=false
    BYPASS_MEMBERSHIP_CHECK=false
    USE_GENERATORS_PARALLEL=false
    GENERATORS_PARALLELISM_LEVEL=16

If you wish to attach a profiler (like visualvm), you may need to add the switch -XX:+StartAttachListener to run.sh

#### Cluster mode

TODO
