#!/bin/sh

for i in PrimeSieve*.lisp; do
  sbcl --script $i
done
