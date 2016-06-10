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

     git clone https://github.com/agoravoting/agora-mixnet.git

     apt-get install rng-tools

     install sbt - http://www.scala-sbt.org/download.html

You must have java installed and JAVA_HOME set.

Running it
----------

You need to have sbt in your path. Then from the root of the agora-mixnet repo

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

These are the steps to configure a production deployment. All the authorities will be configured in the first server. The authorities are not executed by different parties, just like nvotes does with the old SaaS deployment.

First follow the deployment steps in [the tutorial](https://github.com/agoravoting/agora-dev-box/blob/next/doc/production_deployment.md) to configure an agora/auth1/auth2 production deployment. 

Now modify the agora machine to redirect the elections/api requests to agora-mixnet:

     cd /agora/agora
     vagrant ssh
     sudo vim /etc/nginx/conf.d/agora-elections.conf

The agora-elections.conf file should be modified like this (only change the server_name):

     server {
             listen         14443;
             server_name    prod.nvotes.com;
     
             location /public {
                     root /home/agoraelections/datastore;
             }
             location /private {
                     root /home/agoraelections/datastore;
             }
             location / {
                     proxy_pass          http://localhost:9858;
                     proxy_set_header    X-Forwarded-For $remote_addr;
             }
     }
     
     server {
             listen         14444;
             server_name    prod.nvotes.com;
     
             location /public {
                     root /home/agoraelections/datastore;
             }
             location /private {
                     root /home/agoraelections/datastore;
             }
             location / {
                     proxy_pass          http://localhost:9000;
                     proxy_set_header    X-Forwarded-For $remote_addr;
             }
     }
save the file ( :wq ) and then:

Then the /etc/nginx/conf.d/oneserver.conf should be modified so that after these lines:

    # agora_elections
    location /elections/ {
            proxy_pass http://127.0.0.1:14443/;
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    }

You add these other lines:

    # agora_elections old
    location /elections-old/ {
            proxy_pass http://127.0.0.1:14444/;
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    }

Then restart nginx:

     sudo service nginx restart

To create and administrate an election we do it through the administrative interface. Because the administrative interface is not integrated with the new backend yet, we have to configure the administrative interface to use the old api. To do so, edit the file /home/agoragui/agora-gui-admin/avConfig.js and change the electionsAPI accordingly:

     sudo sed -i -e 's/electionsAPI: "\(.*\)\/elections\/api\/",$/electionsAPI: "\1\/elections-old\/api\/",/g' /home/agoragui/agora-gui-admin/avConfig.js 

In the agora machine, on the folder /agora, clone the github projects agora-board and agora-mixnet:

     cd /agora
     git clone https://github.com/agoravoting/agora-mixnet.git
     git clone https://github.com/agoravoting/agora-board.git

you need to install sbt, lightbend activator, and  rng-tools:

http://www.scala-sbt.org/
https://www.lightbend.com/activator/download

I normally have the sbt and activator on ~/app/ and configure ./bashrc to have sbt and activator bins on $PATH

Also, don't forget:

     sudo apt-get install rng-tools

You also need to have a /agora/agora-mixnet/libsigar-amd64-linux.so Download the latest libsigar (yest, it's 1.6.4 from 2012) zip from the following url, unzip the zip file and copy the libsigar-amd64-linux.so to /agora/agora-mixnet/
https://sourceforge.net/projects/sigar/files/

Normally you would also use the Eclipse for scala. I also copy it to ~/app/ :
http://scala-ide.org/download/sdk.html

If you want to open the agora-mixnet and agora-board projects on scala-eclipse IDE, you have to create the eclipse projects:

     cd /agora/agora-mixnet && sbt eclipse
     cd /agora/agora-board && activator eclipse

Then just import the projects in eclipse

Copy the shell scripts on agora-mixnet to the root agora-mixnet folder:

     cd /agora/agora-mixnet && cp src/main/shell/*.sh . && chmod +x *.sh

Configure the PEM cert file used by the agora machine. Enter for example on https://agora/admin/login with a browser, add the cert exception if required, and export the certificate authority of the webpage in PEM format saving it as /agora/agora-board/conf/agora-server.crt

Install docker and docker-compose on the host machine following the guide:
https://docs.docker.com/compose/install/

Compile the projects:

     cd /agora/agora-board/
     activator compile

     cd /agora/agora-mixnet
     sbt compile && sbt assembly

Now we can run the demo. Open 5 terminals:

Terminal 1:

     cd /agora/agora-board/conf
     sudo docker-compose up

Terminal 2:

     cd /agora/agora-board/
     activator run

Terminal 3

     cd /agora/agora-mixnet
     sbt assembly
     ./director.sh

Terminal 4

     cd /agora/agora-mixnet
     ./auth1.sh

Terminal 5

     cd /agora/agora-mixnet
     ./auth2.sh

If you need to rerun the demo, on terminal 1 do:

     sudo docker-compose stop
     sudo docker-compose rm -f
     sudo docker-compose up

and on the other terminals simply do exactly the same. 

You should be able to create elections graphically through the administrative interface, which creates the elections in the old api. Then, executing the following command you can duplicate the election in the new api. Edit the files /agora/agora-mixnet/create_election.py and /agora/agora-mixnet/tally_election.py files to reflect the FQDN of the domain, and then once you have created a new election, just duplicate it knowning only the election id:

     /agora/agora-mixnet/create_election.py 45

That command would create the election 45 with the new mixnet. Now you can add through the administrative interface new electors and send them authentication messages if it's a closed-census election, or they can register directly as new electors through the public election interface.

When ballot is successfully casted, a POST to url https://agora/elections/api/election/3/voter/b31a24a7e75fb324b23069b0150f will be processed by the director on terminal 3, with output similar to:

    Router /api/election/3/voter/b31a24a7e75fb324b23069b0150f
    addVote HMac check : true
    +GG VotesToPostRequest: 
    GG accumulate
    GG PostOffice::add
    GG PostOffice::remove
    GG ElectionStateMaintainer:push
    GG ElectionStateMaintainer::pushVotes
    GG ElectionSubscriber::pull electionType app.Election[shapeless.nat._2,app.Votes[0]]
    GG ElectionStateMaintainer::addVotes
    GG ElectionSubscriber::push electionType app.Election[shapeless.nat._2,app.Votes[1]] uid 3

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
