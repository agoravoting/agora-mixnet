Agora mixnet
============

This is a mixnet voting prototype based around [unicrypt](https://github.com/bfh-evg/univote2) and code/design from [univote](https://github.com/bfh-evg/univote2).

### Cryptography

* encrypting votes
* creating keyshares, proofs and verification
* shuffling votes, proofs and verification
* joint (partial) decryption, proofs and verification

### Performance



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

### Production deployment

The recommended production deployment method is to use docker for orion and mongodb. Docker can have persistence of the data using volumes as mentioned [in the documentation](https://docs.docker.com/engine/userguide/containers/dockervolumes/). To do so, follow these steps:

Create a data volume for mongodb:

     sudo docker create -v /data/db --name mongodb_data mongo:3.2

Start orion and mongodb docker images:

    sudo docker run --name mongodb --volumes-from mongodb_data -d mongo:3.2 --smallfiles --nojournal
    sudo docker run -d --name orion1 --link mongodb:mongodb -p 1026:1026 fiware/orion -dbhost mongodb

Create a backup:

    sudo docker run --volumes-from mongodb_data -v $(pwd):/backup busybox tar cvf /backup/backup.tar /data/db
    
Once a backup has been done, you can stop the docker container and even remove it:

    sudo docker stop mongodb orion1
    sudo docker rm -f mongodb orion1

Because you can run it and restore it later from the backup:

    sudo docker run -v /data/db --name mongodb_data2 mongo:3.2 /bin/bash
    sudo docker run --rm --volumes-from mongodb_data2 -v $(pwd):/backup mongo:3.2 bash -c "cd /data/db && tar xvf /backup/backup.tar --strip 2"
    sudo docker run --name mongodb --volumes-from mongodb_data2 -d mongo:3.2 --smallfiles --nojournal
