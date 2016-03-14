#!/bin/sh

USE_GMP=true
USE_EXTRACTOR=true
BYPASS_MEMBERSHIP_CHECK=true
USE_GENERATORS_PARALLEL=true
GENERATORS_PARALLELISM_LEVEL=16
CLUSTER_SEED=localhost:2555

CPATH=target/scala-2.11/sandbox_2.11-0.1-SNAPSHOT.jar:target/scala-2.11/sandbox-assembly-0.1-SNAPSHOT-deps.jar

# GC_LOGGING="XX:+PrintGCDetails -XX:+PrintGCDateStamps -Xloggc:gc.log"
OPTIONS="-Dmpservice.use-gmp=$USE_GMP -Dmpservice.use-extractor=$USE_EXTRACTOR -Dbypass-membership-check=BYPASS_MEMBERSHIP_CHECK -Duse-generators-parallel=USE_GENERATORS_PARALLEL -Dgenerators-parallelism-level=GENERATORS_PARALLELISM_LEVEL -Dakka.cluster.seed-nodes.0=akka.tcp://ClusterSystem@$CLUSTER_SEED"

java $OPTIONS -d64 -Xms1G -Xmx1G -Djava.library.path=. -Dmaster.max-chunk-size=1000 -Dmaster.min-chunk=6 -classpath $CPATH ElectionTest $*