Agora mixnet
============

This is a mixnet voting prototype based around [unicrypt](https://github.com/bfh-evg/univote2) and code/design from [univote](https://github.com/bfh-evg/univote2).

### Cryptography

* encrypting votes
* creating keyshares, proofs and verification
* shuffling votes, proofs and verification
* joint (partial) decryption, proofs and verification

### Performance

Effort has gone into making the prototype reasonably performant. The following are the main optimization areas and corresponding environment variables to use in execution scripts.

##### Native libgmp modpow implementation

The [jna-gmp](https://github.com/square/jna-gmp) library is used to speed up modular exponentiation.

Switch:

    -Dmpservice.use-gmp=true

##### Protocol level parallelism
Protocol overlaps where computations occur simultaneously are simulated with Futures and Future composition.

![protocol](https://github.com/agoravoting/sandbox/blob/master/doc/protocol.png "protocol")

The implementation covers parallelism at the mixing and decryption phase.

##### Bypassing redundant membership checks
Some group membership checks during deserialization can be skipped as they are detected by the verifier. This saves some costly modular exponentiation operations found at:

        value.modPow(this.getOrder(), this.modulus).equals(MathUtil.ONE);

Switch:

    -Dbypass-membership-check=true

##### Parallel generator computation

The computation of random generators with a deterministic random byte sequence involves calculating hashes sequentially on a single thread which incurs small but noticeable performance penalty. This can be done in parallel seeding the sequences with different numbers.

Switch:

    -Duse-generators-parallel=true

##### Parallel modular exponentiation
Voting systems are inherently very parallelizable as much of the processing is done per-vote in an independent way. The bulk of computation in public key cryptography and related voting systems is modular exponentiation. The task is then to parallelize these costly operations. Unfortunately, extracting parallelism from code that was not designed with it as a central concern from the very beginning is usually very difficult or outright not practical. In this particular case the difficulty is manifested as:

1) the are many different callstacks using modpow

2) modpows occur deep in the callstack, whereas the context where they can be made parallel (loops) is much higher up

3) modpow calculations are typically used immediately after, which makes the vectorization harder as it has to create a boundary between the two

On way to solve this would be to do a full rewrite with parallelization in mind. But this is not practical for this prototype.

The approach used instead was based on an automatic parallelism extraction mechanism that works by monitoring threads of execution at specific code blocks and intercepting modular exponentiation calls. These calls are collected, computed in bulk, and then replayed back to the inspected thread along the specified code block. Because both java8 and scala support lambdas it is possible to represent these code blocks as higher order functions. For automatic extraction to work, these higher order functions must be purely functional. Parallelism and clustering is then achieved via scala collections and akka.

Switch:

    -Dmpservice.use-extractor=$USE_EXTRACTOR
##### Clustered modular exponentiation
Modular exponentiations extracted with the above method are computed across an akka cluster.

##### Other possible optimizations
Performance results suggest that a large portion of computations are now executed in parallel. However a large factor of improvement probably remains. Some ideas:

* Stream modpows instead of waiting till bulk
* Allow extracting modpows from parallel collections
* MPIR/use gmp for multiplying
* JVM/gc/parallelism/akka tuning
* Convert non modpow loops in unicrypt to parallel

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
