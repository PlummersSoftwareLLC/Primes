# Base R implementation of prime sieve

This implementation of the prime sieve is less faithful to the original C++ object-oriented design, going for a more 
functional design, which is more common for R. Part of R's power is through vectorisation (which translates closer to 
C-style for loops) rather than R's built-in for loops, which are usually pretty slow.