# Wren implementation

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

## Run 

### Run locally

Wren CLI is available for download for Linux, macOS, and Windows at https://github.com/wren-lang/wren-cli/releases. After download, make sure the `wren_cli` executable is in the **PATH**.

```
wren_cli primes.wren
```

### Docker

```
docker build -t primes .
docker run --rm primes
```

## Benchmarks

**Machine:** MacBook Pro (13-inch, 2018, Four Thunderbolt 3 Ports)<br/>
**Processor:** 2.3 GHz Quad-Core Intel Core i5<br/>
**Memory:** 16GB 2133 MHz LPDDR3<br/>

```
marghidanu;104;5.002599;1;algorithm=base,faithful=yes
marghidanu;106;5.024289;1;algorithm=base,faithful=yes
marghidanu;101;5.002652;1;algorithm=base,faithful=yes
marghidanu;105;5.030542;1;algorithm=base,faithful=yes
marghidanu;103;5.021849;1;algorithm=base,faithful=yes
marghidanu;95;5.038965;1;algorithm=base,faithful=yes
marghidanu;91;5.016224;1;algorithm=base,faithful=yes
marghidanu;94;5.011683;1;algorithm=base,faithful=yes
marghidanu;99;5.010726;1;algorithm=base,faithful=yes
marghidanu;96;5.062005;1;algorithm=base,faithful=yes
```

**Machine:** MacBook Air (M1, 2020)<br/>
**Processor:** Chip Apple M1<br/>
**Memory** 8GB<br/>

Wren CLI is not yet available for M1, so the following results are from running it under Rosetta 2.

## Notes

Interesting scripting language with concurrency support; I wish the standard library had more functionality. Very easy to use, documentation is clean and easy to understand. Keep in mind that Wren is intended for embedding in applications.

More details at https://wren.io/.

## Author

Tudor Marghidanu
https://marghidanu.com/