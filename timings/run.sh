#!/bin/bash

# This file is part of agora-mixnet.
# Copyright (C) 2015-2016  Agora Voting SL <agora@agoravoting.com>

# agora-mixnet is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License.

# agora-mixnet  is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.

# You should have received a copy of the GNU Lesser General Public License
# along with agora-mixnet.  If not, see <http://www.gnu.org/licenses/>.

# you must have run the assembly command from sbt for this to work
CLASSPATH=../target/scala-2.11/sandbox-assembly-0.1-SNAPSHOT.jar

# leave this if you dont want to test clustering
CLUSTER_SEED=localhost:2555

# the first options to test
OPTIONS_ONE="-Dmpservice.use-gmp=true -Dmpservice.use-extractor=false -Dbypass-membership-check=false -Duse-generators-parallel=false -Dakka.cluster.seed-nodes.0=akka.tcp://ClusterSystem@$CLUSTER_SEED"

# the second  options to test
OPTIONS_TWO="-Dmpservice.use-gmp=false -Dmpservice.use-extractor=false -Dbypass-membership-check=false -Duse-generators-parallel=false -Dakka.cluster.seed-nodes.0=akka.tcp://ClusterSystem@$CLUSTER_SEED"

# space sperated list of vote counts to run
RUNS="100 200"

# note that previous runs are not deleted, this allows incrementally adding data to the file
# but you must manually delete it if you want to overwrite
cp ./times.dat ./times.dat.bak 2>/dev/null || :

# run it
for votes in $RUNS
do
  echo running votes = $votes
  time1=`java $OPTIONS_ONE -classpath $CLASSPATH ElectionTest $votes | grep -Po '(?<=mixTime: )[^\] ]*'`
  time2=`java $OPTIONS_TWO -classpath $CLASSPATH ElectionTest $votes | grep -Po '(?<=mixTime: )[^\] ]*'`
  echo $votes $time1 $time2
  echo $votes $time1 $time2  >> times.dat
done
