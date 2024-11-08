@ECHO OFF

SET "_RUN_ARRAY=0"
IF [%1] == [] SET "_RUN_ARRAY=1"
IF [%1] == [1] SET "_RUN_ARRAY=1"
IF [%1] == [array] SET "_RUN_ARRAY=1"
IF %_RUN_ARRAY% == 1 (
    ECHO Building and running the array approach...
    ECHO:
    g++ -Ofast PrimeCPP_array.cpp -std=c++17 -lstdc++ -oPrimes_array.exe
    .\Primes_array.exe
    ECHO:
)
SET _RUN_ARRAY=

SET "_RUN_MASK=0"
IF [%1] == [] SET "_RUN_MASK=1"
IF [%1] == [2] SET "_RUN_MASK=1"
IF [%1] == [mask] SET "_RUN_MASK=1"
IF %_RUN_MASK% == 1 (
    ECHO Building and running the mask approach...
    ECHO:
    g++ -Ofast PrimeCPP_mask.cpp -std=c++17 -lstdc++ -oPrimes_mask.exe
    .\Primes_mask.exe
    ECHO:
)
SET _RUN_MASK=
