# NetRexx solution by joss
Sieve of Eratosthenes for the NetRexx language.
Uses the RexxLA (Rexx Language Association) translator to compile the program PrimeNetRexx.NetRexx

NetRexx translates source code into Java compiled classes. The NetRexx syntax is similar to REXX,
but there are enough differences to be a separate language. For example: stem variables are not
supported in NetRexx and classes, methods and properties are not supported in REXX.

A Dockerfile has been supplied. It is based upon an openjdk11 image running alpine.

Notes:
In the sieve I tried having the iterators "i" and "j" be both NetRexx types and native java
types (by declaring them as int before running the loops). The NetRexx types version of
PrimeNetRexx returned iteration counts in the range 110-120. The java version returned an
iteration count 35.

While NetRexx does generate java classes, the resultant classes do not have the iteration
counts shown by the drag-race native java programs.

## Run instructions
The supplied Dockerfile runs the default test:

ENTRYPOINT ["java","-cp","lib/NetRexxF.jar:.","PrimeNetRexx"]

PrimeNetRexx takes 3 parameters:
The first parameter (default = 1000000) is the target (highest value to test for prime-acy)
The second parameter (default=0) is an output mode. The following output modes can be used:
0   The standard test as outlined in the "CONTRIBUTING.md" file

1   Print all the primes less-than-or-equal-to the target ("1000000")
    
    docker run --rm --entrypoint "java" ubuntu_netrexx -cp lib/NetRexxF.jar:. PrimeNetRexx 1000000 1

2   (summary) Print only the count of primes less-than-or-equal-to 100, 1000, 10000, 100000 and 1000000
    The summary lines depend on what target value is. Thus if a target of 89000 is specified, then
    the count of primes less-than-or-equal-to 100, 1000, 10000 and 89000 will be listed.
    
    docker run --rm --entrypoint "java" ubuntu_netrexx -cp lib/NetRexxF.jar:. PrimeNetRexx 1000000 2

3   Print the list of primes (mode 1) and then the summary (mode 2) 
   docker run --rm --entrypoint "java" ubuntu_netrexx -cp lib/NetRexxF.jar:. PrimeNetRexx 1000000 3

All output modes print the <label>;<iterations>;<total_time>;<num_threads>;<tags> line outlined in
the "CONTRIBUTING.md" file.


## Output
joss_NetREXX;118;5.065878;1;algorithm=base,bits=8,faithful=no

Notes
o   although the source code creates a class PrimeNetRexx, the program is simply a set of static methods run from main(). For this
    reason, I marked the runs as faithful=no as no objects are ever instantiated.
o   The NetRexx program shows significant improvement over the interpreted Rexx program (116-118 iterations versus fewer than 20). However, it 
    is still slow compared to true compiled/optimized languages.
    
output mode 1 (partial output shown)
joss_NetREXX;116;5.019622;1;algorithm=base,bits=8,faithful=no
       2       3       5       7      11      13      17      19      23      29      31      37      41      43      47      53      59      61      67      71
      73      79      83      89      97     101     103     107     109     113     127     131     137     139     149     151     157     163     167     173
     179     181     191     193     197     199     211     223     227     229     233     239     241     251     257     263     269     271     277     281
     283     293     307     311     313     317     331     337     347     349     353     359     367     373     379     383     389     397     401     409
     419     421     431     433     439     443     449     457     461     463     467     479     487     491     499     503     509     521     523     541
     547     557     563     569     571     577     587     593     599     601     607     613     617     619     631     641     643     647     653     659
     661     673     677     683     ...

output mode 2
joss_NetREXX;117;5.012450;1;algorithm=base,bits=8,faithful=no
25 primes less than 100 (expected 25)
168 primes less than 1,000 (expected 168)
1,229 primes less than 10,000 (expected 1,229)
9,592 primes less than 100,000 (expected 9,592)
78,498 primes less than 1,000,000 (expected 78,948)
