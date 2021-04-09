c:
	- clang -O3 -flto -march=native PrimeC/main.c && ./a.out
cpp:
	- clang++ -O3 -flto -march=native PrimeCPP/PrimeCPP.cpp && ./a.out 
rust:
	- cd PrimeRs && cargo run -q --release -- -C target-cpu=native 
nim:
	- cd PrimeNim && nimble build --d:danger --passC:-flto --passC:-march=native --silent && ./PrimeNim 
d:
	- cd PrimeD && dub build -b release-nobounds -q --compiler=ldc2 && ./primed 
dotnet:
	 - cd PrimeCS && dotnet run --configuration Release
python:
	- python3 PrimeSievePY/PrimePY.py

run:
	- pip install graph-cli --q
	- echo "impl,primes" > primes.csv
	- make c
	- make cpp
	- make rust
	- make nim
	- make d
	- make dotnet
	- make python
	- graph primes.csv -o primes.png --bar
	
clean:
	- rm a.out PrimeRs/target* PrimeCS/obj/ PrimeNim/PrimeNim PrimeD/primed PrimeD/dub_*  -rf 

print_versions:
	- clang --version
	- clang++ --version
	- dotnet --version
	- rustc --version
	- python3 --version
	- nim --version