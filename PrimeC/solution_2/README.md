# C language solution by Daniel Spångberg

faithful 
parallel-faithful

All these implementations are written in C.
The state of the sieve is stored in a C struct (closest to a class in C). 

There are different optimizations:
1of2 - check only odd integers
8of30 - check only integers not divisible by 2, 3, or 5
48of210 - check only integers not divisible by 2, 3, 5, or 7
480of2310 - check only integers not divisible by 2, 3, 5, 7, or 11
5760of30030 - check only integers not divisible by 2, 3, 5, 7, 11, or 13

Only bits corresponding to odd integers are stored.
The inner loop masking is handled in three different ways:
(1) Write each bit corresponding to the multiple of the found prime
(2) Only write every multiple that will be read by the outer loop bit checking (i.e. only write bits corresponding to integers not divisible by 2, 3, or 5 for the 8of30 algorithm.
(3) Write each bit corresponding to the multiple of the found prime in parallel on each thread, taking care to never touch the same word in two different threads. The implementation uses OpenMP (This is the _par versions)

Also, running the sieves in parallel in different threads in an embarrassingly parallel style is tested, also using OpenMP (The _epar versions)


1of2 - Implementation close to the original implementation of Dave.
8of30 - Implementation writing bits in style (1) above
8of30_owrb - Implementation writing bits in style (2)
48of210 - Implementation writing bits in style (1)
48of210_owrb - Implementation writing bits in style (2)
480of2310_owrb - Implementation writing bits in style (2)
5760of30030_owrb - Implementation writing bits in style (2)
1of2_par - Implementation writing bits in style (3)
8of30_par - Implementation writing bits in style (3)
48of210_par - Implementation writing bits in style (3)
480of2310_par - Implementation writing bits in style (3)
5760of30030_par - Implementation writing bits in style (3)
1of2_epar - Implementation writing bits in style (1), embarrassingly parallel
8of30_epar - Implementation writing bits in style (2), embarrassingly parallel
48of210_epar - Implementation writing bits in style (2), embarrassingly parallel
480of2310_epar - Implementation writing bits in style (2), embarrassingly parallel
5760of30030_epar - Implementation writing bits in style (2), embarrassingly parallel


## Run instructions
You can use the Dockerfile - but the code (especially the parallel implementations) runs (much) slower in docker.

First compile the code using the compile.sh script. Optimization parameters are at the beginning of the script. They are set up to use the gcc compiler.

Then run the code using the run.sh script. (You can change the number of threads in the script, the default runs up to 4 threads on the parallel bit masking and the maximum number of cpus on the embarrassingly parallel implementation).

## Output

On i5-9400 (WSL2, no docker):
```
danielspaangberg_1of2;9404;5.000237;1
danielspaangberg_8of30;15865;5.000086;1
danielspaangberg_8of30_owrb;15765;5.000077;1
danielspaangberg_48of210;18394;5.000217;1
danielspaangberg_48of210_owrb;20996;5.000030;1
danielspaangberg_480of2310_owrb;20492;5.000061;1
danielspaangberg_5760of30030_owrb;30024;5.000047;1
danielspaangberg_1of2_par;19701;5.000116;4
danielspaangberg_8of30_par;25429;5.000175;4
danielspaangberg_48of210_par;26872;5.000039;4
danielspaangberg_480of2310_par;28288;5.000120;4
danielspaangberg_5760of30030_par;29071;5.000140;4
danielspaangberg_1of2_epar;50995;5.000814;6
danielspaangberg_8of30_epar;74803;5.003918;6
danielspaangberg_48of210_epar;98369;5.007262;6
danielspaangberg_480of2310_epar;139872;5.001439;6
danielspaangberg_5760of30030_epar;171797;5.000457;6
```

On i5-9400 (Docker / WSL2):
```
danielspaangberg_1of2;9356;5.000167;1
danielspaangberg_8of30;14837;5.000084;1
danielspaangberg_8of30_owrb;9724;5.000396;1
danielspaangberg_48of210;17078;5.000122;1
danielspaangberg_48of210_owrb;12584;5.000924;1
danielspaangberg_480of2310_owrb;14966;5.000126;1
danielspaangberg_5760of30030_owrb;17565;5.000154;1
danielspaangberg_1of2_par;9075;5.000343;4
danielspaangberg_8of30_par;10068;5.000063;4
danielspaangberg_48of210_par;10411;5.000016;4
danielspaangberg_480of2310_par;10707;5.000258;4
danielspaangberg_5760of30030_par;10925;5.000231;4
danielspaangberg_1of2_epar;52399;5.002040;6
danielspaangberg_8of30_epar;53852;5.003019;6
danielspaangberg_48of210_epar;65535;5.002571;6
danielspaangberg_480of2310_epar;81808;5.002424;6
danielspaangberg_5760of30030_epar;94400;5.001967;6
```

On Raspberry Pi 4b (2GHz, ARM64):
```
danielspaangberg_1of2;3194;5.001091;1
danielspaangberg_8of30;4895;5.000483;1
danielspaangberg_8of30_owrb;5541;5.000325;1
danielspaangberg_48of210;5561;5.000758;1
danielspaangberg_48of210_owrb;6863;5.000571;1
danielspaangberg_480of2310_owrb;8399;5.000213;1
danielspaangberg_5760of30030_owrb;9404;5.000325;1
danielspaangberg_1of2_par;6846;5.000116;4
danielspaangberg_8of30_par;8616;5.000388;4
danielspaangberg_48of210_par;9328;5.000425;4
danielspaangberg_480of2310_par;9608;5.000004;4
danielspaangberg_5760of30030_par;9914;5.000242;4
danielspaangberg_1of2_epar;11058;5.002297;4
danielspaangberg_8of30_epar;16689;5.001790;4
danielspaangberg_48of210_epar;17549;5.001936;4
danielspaangberg_480of2310_epar;18734;5.001499;4
danielspaangberg_5760of30030_epar;18355;5.001650;4
```
