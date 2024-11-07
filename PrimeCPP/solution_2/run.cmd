IF "%1" == "" || "%1" == "1" || "%1" == "array" (
    g++ -Ofast PrimeCPP_array.cpp -std=c++17 -lstdc++ -oPrimes_array.exe
    .\Primes_array.exe
)

IF "%1" == "" || "%1" == "2" || "%1" == "mask" (
    g++ -Ofast PrimeCPP_mask.cpp -std=c++17 -lstdc++ -oPrimes_mask.exe
    .\Primes_mask.exe
)
