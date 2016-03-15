#!/bin/sh

#
# this script must be run from the home directory, so first copy it there
#

USE_GMP=true
CLUSTER_SEED=localhost:2555
HOSTNAME=localhost
PORT=2555

# you must have run the assembly command from sbt for this to work
CPATH=target/scala-2.11/sandbox-assembly-0.1-SNAPSHOT.jar

OPTIONS="-Dmpservice.use-gmp=$USE_GMP -Dakka.remote.netty.tcp.port=$PORT -Dakka.remote.netty.tcp.hostname=$HOSTNAME -Dakka.cluster.seed-nodes.0=akka.tcp://ClusterSystem@$CLUSTER_SEED"

java $OPTIONS -d64 -Xms1G -Xmx1G -Djava.library.path=. -classpath $CPATH mpservice.WorkerApp $*
