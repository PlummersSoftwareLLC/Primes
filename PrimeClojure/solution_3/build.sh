#!/bin/bash

mkdir classes
clojure -M -e "(compile 'sieve)"
clojure -M:uberjar --main-class sieve
