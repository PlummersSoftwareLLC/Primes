del .\PrimeRust.exe
del .\PrimeRust.pdb
rustc -C opt-level=3 PrimeRust.rs
.\PrimeRust.exe
