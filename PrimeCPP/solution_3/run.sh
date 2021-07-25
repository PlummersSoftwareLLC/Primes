clang++ -pthread -O3 -m64 -mtune=native -std=c++20 -fconstexpr-steps=1000000000 -fconstexpr-depth=100000000 PrimeCPP_CONSTEXPR.cpp -oprimes_constexpr.exe
./primes_constexpr.exe
