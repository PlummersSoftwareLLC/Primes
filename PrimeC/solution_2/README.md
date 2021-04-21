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
First compile the code using the compile.sh script. Optimization parameters are at the beginning of the script. They are set up to use the gcc compiler.

Then run the code using the run.sh script. (You can change the number of threads in the script).


## Output

On i5-9400:
```
danielspaangberg_1of2;9312;5.000210;1
danielspaangberg_8of30;15120;5.000302;1
danielspaangberg_8of30_owrb;15826;5.000068;1
danielspaangberg_48of210;17784;5.000126;1
danielspaangberg_48of210_owrb;21011;5.000174;1
danielspaangberg_480of2310_owrb;25198;5.000175;1
danielspaangberg_5760of30030_owrb;24379;5.000065;1
danielspaangberg_1of2_par;19015;5.000244;4
danielspaangberg_8of30_par;24444;5.000158;4
danielspaangberg_48of210_par;25932;5.000165;4
danielspaangberg_480of2310_par;27083;5.000018;4
danielspaangberg_5760of30030_par;28623;5.000176;4
danielspaangberg_1of2_epar;43744;5.004085;6
danielspaangberg_8of30_epar;73162;5.003259;6
danielspaangberg_48of210_epar;99978;5.006717;6
danielspaangberg_480of2310_epar;121115;5.003408;6
danielspaangberg_5760of30030_epar;141217;5.003322;6
```

On Raspberry Pi 4b (2GHz, ARM64):
```
danielspaangberg_1of2;2723;5.001109;1
danielspaangberg_8of30;4591;5.000613;1
danielspaangberg_8of30_owrb;5334;5.000507;1
danielspaangberg_48of210;5223;5.000424;1
danielspaangberg_48of210_owrb;6680;5.000573;1
danielspaangberg_480of2310_owrb;8156;5.000002;1
danielspaangberg_5760of30030_owrb;9103;5.000441;1
danielspaangberg_1of2_par;6354;5.000474;4
danielspaangberg_8of30_par;8134;5.000051;4
danielspaangberg_48of210_par;9012;5.000146;4
danielspaangberg_480of2310_par;8996;5.000284;4
danielspaangberg_5760of30030_par;9500;5.000169;4
danielspaangberg_1of2_epar;9848;5.002853;4
danielspaangberg_8of30_epar;16746;5.002118;4
danielspaangberg_48of210_epar;17178;5.001602;4
danielspaangberg_480of2310_epar;20498;5.001664;4
danielspaangberg_5760of30030_epar;18748;5.001352;4
```
