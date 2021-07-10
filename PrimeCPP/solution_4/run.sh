clang++ -pthread -O3 -m64 -mtune=native -std=c++17 generator.cpp -ogenerator.exe
dir ./generator.exe > primes.h
clang++ -pthread -O3 -m64 -mtune=native -std=c++17 fastware.cpp -ofastware.exe
./fastware.exe