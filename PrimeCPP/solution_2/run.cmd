@echo off

IF "%1" == "" || "%1" == "1" || "%1" == "array" (
    ECHO Building and running the array approach...
    ECHO(
    g++ -Ofast PrimeCPP_array.cpp -std=c++17 -lstdc++ -oPrimes_array.exe
    .\Primes_array.exe
    ECHO(
)

IF "%1" == "" || "%1" == "2" || "%1" == "mask" (
    ECHO Building and running the mask approach...
    ECHO(
    g++ -Ofast PrimeCPP_mask.cpp -std=c++17 -lstdc++ -oPrimes_mask.exe
    .\Primes_mask.exe
    ECHO(
)
