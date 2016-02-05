Sequential Benchmarking
=======================

Run benchmarks with different vote counts and command line options.

Installation
============

You need gnuplot

    apt-get install gnuplot

Java must be on your path

Configuration
=============

At the top of run.sh set the CLASSPATH and RUN variables accordingly. 

     CLASSPATH=../target/scala-2.11/sandbox-assembly-0.1-SNAPSHOT.jar
     RUNS="10 20 50"

RUNS is a space separated list of vote counts that will be run.

Use
===

     ./run.sh

This may take a while depending on the RUNS variable. Once finished a file times.dat will be written. It contains
mix times. You can plot the data with

    gnuplot plot.gpi

which will show the data points as well as a linear fit whose gradient tells you seconds/vote. The graphs are output as png image files.

You probably want to modify run.sh and plot.gpi to configure your own runs and options, currently java modpow and gmp modpow runs are compared.