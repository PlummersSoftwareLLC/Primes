@echo off
setlocal
if exist primes.exe del primes.exe

where g++ >nul 2>nul
if %ERRORLEVEL% EQU 0 (
    g++ -O3 -march=native -mtune=native -pthread -std=c++17 PrimeCPP.cpp -o primes.exe
    primes.exe
    exit /b %ERRORLEVEL%
)

where clang++ >nul 2>nul
if %ERRORLEVEL% EQU 0 (
    clang++ -O3 -march=native -mtune=native -pthread -std=c++17 PrimeCPP.cpp -o primes.exe
    primes.exe
    exit /b %ERRORLEVEL%
)

where cl >nul 2>nul
if %ERRORLEVEL% EQU 0 (
    cl /O2 /std:c++17 /EHsc /permissive- PrimeCPP.cpp /Feprimes.exe
    primes.exe
    exit /b %ERRORLEVEL%
)

echo No suitable C++ compiler found (g++, clang++, or cl).
exit /b 1
