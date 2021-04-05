# Crystal implementation

## Crystal Installation

The installation guide for Crystal is available here: https://crystal-lang.org/install/

## Running under M1

```
MacBook Air (M1, 2020)
Chip Apple M1
Memory 8GB
```

Unfortunately the compiler is not ready for the M1 processor. But I've teststed it on a M1 Macbook Air iside Docker. The application was compiled under the Alpine Docker image generating a **ARM aarch64** executable

```
docker-compose build
docker-compose run --rm primes
```

## Running under Intel

```
Machine: MacBook Pro (13-inch, 2018, Four Thunderbolt 3 Ports)
Processor: 2.3 GHz Quad-Core Intel Core i5
Memory: 16GB 2133 MHz LPDDR3
```

To build 
