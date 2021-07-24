# C language solution by Daniel Spångberg

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

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
danielspaangberg_1of2;10000;5.000305;1;algorithm=base,faithful=yes,bits=1
danielspaangberg_8of30;15852;5.000120;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_8of30_owrb;17487;5.000032;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210;18425;5.000031;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210_owrb;23344;5.000077;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_480of2310_owrb;27994;5.000026;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_5760of30030_owrb;32872;5.000085;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_1of2_par;20197;5.000203;4;algorithm=base,faithful=yes,bits=1
danielspaangberg_8of30_par;24297;5.000176;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210_par;27088;5.000170;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_480of2310_par;28164;5.000065;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_5760of30030_par;29089;5.000059;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_1of2_epar;54939;5.000779;6;algorithm=base,faithful=yes,bits=1
danielspaangberg_8of30_epar;99934;5.000653;6;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210_epar;132728;5.001278;6;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_480of2310_epar;159456;5.000567;6;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_5760of30030_epar;187042;5.002346;6;algorithm=wheel,faithful=yes,bits=1
```

On i5-9400 (Docker / WSL2):
```
danielspaangberg_1of2;9692;5.000474;1;algorithm=base,faithful=yes,bits=1
danielspaangberg_8of30;14777;5.000164;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_8of30_owrb;9639;5.000136;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210;17084;5.000198;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210_owrb;12707;5.000190;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_480of2310_owrb;15219;5.000246;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_5760of30030_owrb;17836;5.000029;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_1of2_par;9024;5.000498;4;algorithm=base,faithful=yes,bits=1
danielspaangberg_8of30_par;10109;5.000448;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210_par;10598;5.000019;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_480of2310_par;10817;5.000096;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_5760of30030_par;11009;5.000264;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_1of2_epar;54120;5.002285;6;algorithm=base,faithful=yes,bits=1
danielspaangberg_8of30_epar;53375;5.002453;6;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210_epar;69328;5.010508;6;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_480of2310_epar;81199;5.003292;6;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_5760of30030_epar;90976;5.002068;6;algorithm=wheel,faithful=yes,bits=1
```

On Raspberry Pi 4b (2GHz, ARM64):
```
danielspaangberg_1of2;3150;5.000062;1;algorithm=base,faithful=yes,bits=1
danielspaangberg_8of30;4711;5.000430;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_8of30_owrb;6348;5.000481;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210;5550;5.000757;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210_owrb;7761;5.000091;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_480of2310_owrb;9065;5.000311;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_5760of30030_owrb;10191;5.000020;1;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_1of2_par;6907;5.000622;4;algorithm=base,faithful=yes,bits=1
danielspaangberg_8of30_par;7629;5.000126;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210_par;8654;5.000219;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_480of2310_par;9420;5.000243;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_5760of30030_par;9434;5.000104;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_1of2_epar;10524;5.002370;4;algorithm=base,faithful=yes,bits=1
danielspaangberg_8of30_epar;16896;5.001638;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_48of210_epar;19032;5.001211;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_480of2310_epar;19070;5.001330;4;algorithm=wheel,faithful=yes,bits=1
danielspaangberg_5760of30030_epar;19314;5.001127;4;algorithm=wheel,faithful=yes,bits=1
```
