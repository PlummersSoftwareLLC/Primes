run:
	- echo "C++:"
	- g++ -O3 -flto -march=native PrimeCPP/PrimeCPP.cpp && ./a.out
	- echo "C#:"
	- cd PrimeCS && dotnet run --configuration Release
	- echo "Rust:"
	- cd PrimeRs && cargo run -q --release -- -C target-cpu=native
	- echo "Python:"
	- python3 PrimeSievePY/PrimePY.py

clean:
	- rm PrimeCPP/a.out PrimeRs/target* PrimeCS/obj/* -rf

print_versions:
	- g++ --version
	- dotnet --version
	- rustc --version
	- python3 --version