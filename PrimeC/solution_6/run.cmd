@ECHO OFF

ECHO Building and running msubra_c_wheel30_extend...
ECHO:
clang -O3 -ffast-math -fno-math-errno -fno-trapping-math -flto -std=c11 PrimeC_extend.c -o Primes_c_extend.exe -lm -lpthread
.\Primes_c_extend.exe
ECHO:
