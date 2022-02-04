#!/bin/bash

javac PrimeSieveJava.java
java PrimeSieveJava -warmup
java PrimeSieveJava -variant bitset -warmup