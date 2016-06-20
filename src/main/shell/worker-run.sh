#!/bin/sh

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
