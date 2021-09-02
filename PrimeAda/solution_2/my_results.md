# Results on my FX-8350 machine

I closed all browsers/email clients and then ran each 4 times.

## PrimeAda/solution_1

This version passes no optimisation flags to the compiler.

```bash
Passes: 685, Time: 5.005589827, Avg:  0.007307430, Limit : 1000000, Count1 : 78498, Count2: 78498, Valid :TRUE

BoopBeepBoopBeep; 685; 5.005589827;1;algorithm=base,faithful=no

Passes: 684, Time: 5.006376276, Avg:  0.007319263, Limit : 1000000, Count1 : 78498, Count2: 78498, Valid :TRUE

BoopBeepBoopBeep; 684; 5.006376276;1;algorithm=base,faithful=no

Passes: 691, Time: 5.001339156, Avg:  0.007237828, Limit : 1000000, Count1 : 78498, Count2: 78498, Valid :TRUE

BoopBeepBoopBeep; 691; 5.001339156;1;algorithm=base,faithful=no

Passes: 669, Time: 5.001479115, Avg:  0.007476052, Limit : 1000000, Count1 : 78498, Count2: 78498, Valid :TRUE

BoopBeepBoopBeep; 669; 5.001479115;1;algorithm=base,faithful=no
```

By building this with the following command:

```bash
gprbuild -p -cargs -O3
```

I get this improvement:

```bash
Passes: 1454, Time: 5.000800410, Avg:  0.003439340, Limit : 1000000, Count1 : 78498, Count2: 78498, Valid :TRUE

BoopBeepBoopBeep; 1454; 5.000800410;1;algorithm=base,faithful=no

Passes: 1453, Time: 5.000395932, Avg:  0.003441428, Limit : 1000000, Count1 : 78498, Count2: 78498, Valid :TRUE

BoopBeepBoopBeep; 1453; 5.000395932;1;algorithm=base,faithful=no

Passes: 1453, Time: 5.001075624, Avg:  0.003441896, Limit : 1000000, Count1 : 78498, Count2: 78498, Valid :TRUE

BoopBeepBoopBeep; 1453; 5.001075624;1;algorithm=base,faithful=no

Passes: 1451, Time: 5.001357837, Avg:  0.003446835, Limit : 1000000, Count1 : 78498, Count2: 78498, Valid :TRUE

BoopBeepBoopBeep; 1451; 5.001357837;1;algorithm=base,faithful=no
```

## PrimeCPP/solution_1

```bash
Passes: 3859, Time: 5.000600, Avg: 0.001296, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1

davepl;3859;5.000600;1;algorithm=base,faithful=yes,bits=1

Passes: 3826, Time: 5.000307, Avg: 0.001307, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1

davepl;3826;5.000307;1;algorithm=base,faithful=yes,bits=1

Passes: 3833, Time: 5.001232, Avg: 0.001305, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1

davepl;3833;5.001232;1;algorithm=base,faithful=yes,bits=1

Passes: 3827, Time: 5.000427, Avg: 0.001307, Limit: 1000000, Count1: 78498, Count2: 78498, Valid: 1

davepl;3827;5.000427;1;algorithm=base,faithful=yes,bits=1
```

## PrimeC/solution_1

```bash
mckoss-c830;8173;5.0;1;algorithm=wheel,faithful=yes,bits=1

mckoss-c830;8225;5.0;1;algorithm=wheel,faithful=yes,bits=1

mckoss-c830;8209;5.0;1;algorithm=wheel,faithful=yes,bits=1

mckoss-c830;8140;5.0;1;algorithm=wheel,faithful=yes,bits=1
```

## PrimePascal/solution_1

Going by the Dockerfile, this implementation also doesn't pass any optimisation flags to the compiler.

```bash
rbergen;1853;5.00;1;algorithm=base,faithful=yes

rbergen;1868;5.00;1;algorithm=base,faithful=yes

rbergen;1861;5.00;1;algorithm=base,faithful=yes

rbergen;1851;5.00;1;algorithm=base,faithful=yes
``
