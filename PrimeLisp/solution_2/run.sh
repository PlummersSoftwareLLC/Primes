#!/bin/sh

# run base
sbcl --script PrimeSieve.lisp


# run wheel
sbcl --script PrimeSieveWheel.lisp
