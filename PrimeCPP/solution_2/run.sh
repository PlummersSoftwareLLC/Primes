# g++ -Ofast  -std=c++17 -lc++ PrimeCPP.cpp -oPrimes.exe
# gcc -Ofast -std=c++17 PrimeCPP.cpp -lc++ -oPrimes_gcc.exe
# clang -Ofast -std=c++17 -lc++ PrimeCPP.cpp -oPrimes_clang.exe

clang++ -pthread -Ofast -std=c++17 PrimeCPP_PAR.cpp -oprimes_par.exe
./primes_par.exe
