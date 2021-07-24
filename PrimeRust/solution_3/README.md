# Rust solution by Blui42

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

This is mostly a 1:1 implementation of the C++ code in rust.
Uncomment line 61 for an output of all the primes.

## Run instructions

This requires rust to be installed.

```cargo run --release```

This will compile and run the program.

The executable can afterwards be found and executed in `/target/release/`.

## Output

This was run on a Windows Machine with AMD Ryzen 5 1600 Six-Core Processor 3.20 GHz.

```
Author; Passes; Time; Threads
Blui42; 4085; 5.0007594; 1
816.88 Passes per second
For Validation: 78498 Primes were found
```

On wsl:

```
Author; Passes; Time; Threads
Blui42; 4032; 5.000549; 1
806.31 Passes per second
For Validation: 78498 Primes were found
```

Only line 2 is on standard output, the rest is on standard Error.
