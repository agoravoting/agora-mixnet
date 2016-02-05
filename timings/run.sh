#!/bin/bash

CLASSPATH=../target/scala-2.11/sandbox-assembly-0.1-SNAPSHOT.jar
RUNS="10 20 50 100 200 400"
# RUNS=100 200 400 800 2000 4000

cp ./times.dat ./times.dat.bak 2>/dev/null || :
for votes in $RUNS
do
  echo running votes = $votes
  time=`java -classpath $CLASSPATH ElectionTest $votes | grep -Po '(?<=mixTime: )[^\] ]*'`
  time_gmp=`java -classpath $CLASSPATH ElectionTest $votes gmp | grep -Po '(?<=mixTime: )[^\] ]*'`
  echo $votes $time $time_gmp
  echo $votes $time $time_gmp >> times.dat
done