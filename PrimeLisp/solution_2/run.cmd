@echo off

for %%i in (PrimeSieve*.lisp) do @sbcl --script %%i done
