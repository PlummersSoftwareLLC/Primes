# REXX solution by joss
Sieve of Erastosthones for the REXX scripting language.
Uses the Regina REXX interpreter to run the program /home/rexx/PrimeREXX.rex
A Dockerfile has been supplied. It is based upon an Ubuntu container.

Regina REXX processes standard REXX. Standard REXX does not have classes/objects.
There is a separate REXX interpreter: oorexx that does support classes.

I set faithful=yes even though the solution does not use a class to run the sieve.
## Run instructions
The supplied Dockerfile runs the default test:

ENTRYPOINT ["rexx", "./PrimeREXX", "1000000", "0"]

The first parameter ("1000000") is the target (highest value to test for prime-acy)
The second parameter "0" is an output mode. The following output modes can be used:
0   The standard test as outlined in the "CONTRIBUTING.md" file

1   Print all the primes less-than-or-equal-to the target ("1000000")

2   (summary) Print only the count of primes less-than-or-equal-to 100, 1000, 10000, 100000 and 1000000
    The summary lines depend on what target value is. Thus if a target of 89000 is specified, then
    the count of primes less-than-or-equal-to 100, 1000, 10000 and 89000 will be listed.

3   Print both the summary (mode 2) and the list of primes (mode 1)

All output modes print the <label>;<iterations>;<total_time>;<num_threads>;<tags> line outlined in
the "CONTRIBUTING.md" file.


## Output
joss_REXX;12;5.047742;1;algorithm=base,bits=8,faithful=yes,parallel=no
