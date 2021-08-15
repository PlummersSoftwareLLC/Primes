# Emojicode implementation

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

## Run

### Run locally

Emojicode is available for macOS and Linux at: https://www.emojicode.org/docs/guides/install.html.

### Docker

```
docker build -t primes .
docker run --rm primes
```

## Benchmarks

```
Passes: 18, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498
Passes: 18, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498
Passes: 19, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498
Passes: 18, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498
Passes: 19, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498
Passes: 17, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498
Passes: 19, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498
Passes: 18, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498
Passes: 19, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498
Passes: 18, Time: 5, Avg: 0, Limit: 1000000, Count1: 78498, Count2: 78498
```

## Notes

WOW, I can't believe I did this. It took me about 4 hours to write this from scratch; although it looks exciting, coding with emojis can give you a headache. Nevertheless is a fascinating language; since it's still in beta, there are quite a few limitations:

* There is no break/continue on loops. I had to simulate one for this implementation.
* Due to the unusual syntax, you can make mistakes very easily.

## Author

Tudor Marghidanu
https://marghidanu.com/