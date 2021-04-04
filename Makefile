run:
	- g++ -O3 -flto PrimeCPP/PrimeCPP.cpp && ./a.out
	- cd PrimeCS && dotnet run --configuration Release
	- cd PrimeRs && cargo run -q --release
	- python3 PrimeSievePY/PrimePY.py

clean:
	- rm PrimeCPP/a.out PrimeRs/target* PrimeCS/obj/* -rf

print_versions:
	- g++ --version
	- dotnet --version
	- rustc --version
	- python3 --version