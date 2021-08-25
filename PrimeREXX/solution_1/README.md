# REXX solution by joss
Sieve of Erastosthones for the REXX scripting language.
Uses the Regina REXX interpreter to run the program /home/rexx/PrimeREXX.rex
A Dockerfile has been supplied. It is based upon an Ubuntu:20.04 image.

Regina REXX processes standard REXX. Standard REXX does not have classes/objects.
There is a separate REXX interpreter: oorexx that does support classes.

## Run instructions
The supplied Dockerfile runs the default test:

ENTRYPOINT ["rexx", "./PrimeREXX", "1000000", "0"]

The first parameter ("1000000") is the target (highest value to test for prime-acy)
The second parameter "0" is an output mode. The following output modes can be used:
0   The standard test as outlined in the "CONTRIBUTING.md" file

1   Print all the primes less-than-or-equal-to the target ("1000000")
    
    docker run --entrypoint "rexx" ubuntu_rexx .\PrimeREXX.rex 1000000 1

2   (summary) Print only the count of primes less-than-or-equal-to 100, 1000, 10000, 100000 and 1000000
    The summary lines depend on what target value is. Thus if a target of 89000 is specified, then
    the count of primes less-than-or-equal-to 100, 1000, 10000 and 89000 will be listed.
    
    docker run --entrypoint "rexx" ubuntu_rexx .\PrimeREXX.rex 1000000 2

3   Print both the summary (mode 2) and the list of primes (mode 1)
    docker run --entrypoint "rexx" ubuntu_rexx .\PrimeREXX.rex 1000000 3

All output modes print the <label>;<iterations>;<total_time>;<num_threads>;<tags> line outlined in
the "CONTRIBUTING.md" file.


## Output
joss_REXX;21;5.001000;1;algorithm=base,bits=8,faithful=no

output mode 1 (partial output shown)
joss_REXX;21;5.015000;1;algorithm=base,bits=8,faithful=no
       2       3       5       7      11      13      17      19      23      29      31      37      41      43      47      53      59      61      67      71
      73      79      83      89      97     101     103     107     109     113     127     131     137     139     149     151     157     163     167     173
     179     181     191     193     197     199     211     223     227     229     233     239     241     251     257     263     269     271     277     281
     283     293     307     311     313     317     331     337     347     349     353     359     367     373     379     383     389     397     401     409
     419     421     431     433     439     443     449     457     461     463     467     479     487     491     499     503     509     521     523     541
     547     557     563     569     571     577     587     593     599     601     607     613     617     619     631     641     643     647     653     659
     661     673     677     683 ...

output mode 2
joss_REXX;21;5.055000;1;algorithm=base,bits=8,faithful=no
25 primes less than 100
168 primes less than 1,000
1229 primes less than 10,000
9592 primes less than 100,000
78498 primes less than 1,000,000
