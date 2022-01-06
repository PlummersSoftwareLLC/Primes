/*
Deno implementation of Prime Sieve
Based on:
- implementation of NodeJS/solution_1 by Rogier van Dam
- previous implementation of NodeJS/solution_1 by Frank van Bakel
- Python/solution_2 by ssovest
- MyFirstPython Program (tm) Dave Plummer 8/9/2018

Author:    Petru Szemereczki
Date:      2022-01-04
*/

// Historical data for validating our results - the number of primes
// to be found under some limit, such as 168 primes under 1000
const knownPrimeCounts: Record<number, number> = {
  10: 4,
  100: 25,
  1_000: 168,
  10_000: 1_229,
  100_000: 9_592,
  1_000_000: 78_498,
  10_000_000: 664_579,
  100_000_000: 5_761_455,
};

// 32-bit bitarray for javascript, with only needed functions
// int32, not uint and not 64bit because: javascript uses 32int
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Bitwise_AND
// shifting not with >> but with >>> is for zero fill right shift
class BitArray {
  private readonly wordArray: Int32Array;

  constructor(size: number) {
    this.wordArray = new Int32Array(1 + (size >>> 5)); // allocation in javascript is with 0
  }

  setBitTrue(index: number): void {
    const wordOffset = index >>> 5; // 1 word = 2ˆ5 = 32 bit, so shift 5, much faster than /32
    const bitOffset = index & 31; // use & (and) for remainder, faster than modulus of /32
    this.wordArray[wordOffset] |= 1 << bitOffset;
  }

  testBitTrue(index: number): number {
    const wordOffset = index >>> 5;
    const bitOffset = index & 31;
    return this.wordArray[wordOffset] & (1 << bitOffset); // use a mask to only get the bit at position bitOffset. >0=true, 0=false
  }
}

/*
Main class for the prime calculation.
The BitArray stores only odd numbers, with formula number(index) = 2*index+1, e.g.
index = 0 -> number = 1
index = 1 -> number = 3
*/
class PrimeSieve {
  private readonly sieveSize: number;
  private readonly oddsize: number;
  private readonly bitarray: BitArray;

  constructor(sieveSize: number) {
    this.sieveSize = sieveSize;
    this.oddsize = sieveSize >>> 1;
    this.bitarray = new BitArray(1 + this.oddsize);
  }

  runSieve(): void {
    const q = Math.ceil(Math.sqrt(this.oddsize)); // convert to integer with ceil

    for (let factor = 1; factor <= q; factor++) {
      if (!this.bitarray.testBitTrue(factor)) {
        const step = factor * 2 + 1;
        const start = factor * factor * 2 + factor + factor;

        for (
          let multiple = start;
          multiple < this.oddsize;
          multiple = multiple + step
        ) {
          this.bitarray.setBitTrue(multiple); // mark every multiple of this prime
        }
      }
    }
  }

  countPrimes(): number {
    let total = 1; // account for prime 2
    for (let index = 1; index < this.oddsize; index++) {
      if (!this.bitarray.testBitTrue(index)) { // if bit is false, it's a prime, because non-primes are marked true
        total++;
      }
    }
    return total;
  }

  getPrimes(max = 100): number[] {
    const primes = [2]; // 2 is a special prime
    let count = 0;
    for (let factor = 1; factor < this.oddsize; factor++) {
      if (count >= max) break;
      if (!this.bitarray.testBitTrue(factor)) {
        count = primes.push(factor * 2 + 1);
      }
    }
    return primes;
  }
}

// run the sieve for timeLimitSeconds
function runSieveBatch(sieveSize: number, timeLimitSeconds = 5) {
  let nrOfPasses = 0; // Counter for the number of passes in a from timestart to timefinish

  const timeStart = performance.now(); // Record starting time
  const timeFinish = timeStart + timeLimitSeconds * 1_000; // Calculate finish time before, so we don't repeat

  let sieve; // outside do loop to reference the last instance in verbose output
  do {
    sieve = new PrimeSieve(sieveSize);
    sieve.runSieve();
    nrOfPasses++;
  } while (performance.now() < timeFinish); // keep going for timeLimitSeconds

  return nrOfPasses;
}

// get a single sieve (for validation and statistics)
function evalSieve(sieveSize: number, maxShowPrimes = 100) {
  const sieve = new PrimeSieve(sieveSize);
  sieve.runSieve();
  return {
    "countedPrimes": sieve.countPrimes(),
    "primeArray": sieve.getPrimes(maxShowPrimes),
  };
}

/*
main procedure
*/

function main(): void {
  // run once, without threads
  const verbose = false;
  const config = {
    maxShowPrimes: verbose ? 100 : 0,
  };
  const nrOfPasses = 1;

  const sieveSize = 1000000;
  const timeLimitSeconds = 5;

  //measure time running the batch
  const timeStart = performance.now();
  const totalPasses = runSieveBatch(sieveSize, timeLimitSeconds);
  const timeEnd = performance.now();
  const durationInMs = timeEnd - timeStart;
  const durationInSec = durationInMs / 1000;

  // validate algorithm - run one final time on the result
  const sieveResult = evalSieve(sieveSize);
  let validResult = false;
  if (sieveSize in knownPrimeCounts) {
    const knownPrimeCount = knownPrimeCounts[sieveSize];
    validResult = knownPrimeCount === sieveResult["countedPrimes"];
    if (!validResult) {
      console.log(
        `\nError: invalid result. Limit for ${sieveSize} should be ${knownPrimeCount} but result contains ${
          sieveResult["countedPrimes"]
        } primes`,
      );
    }
  } else {
    console.log(
      `Warning: cannot validate result of ${
        sieveResult["countedPrimes"]
      } primes: limit ${sieveSize} is not in the known list of number of primes!`,
    );
  }

  if (validResult) {
    const scriptOrCompiled = Deno.env.get("IS_COMPILED") === "0"
      ? "-script"
      : "-compiled";
    console.log(
      `\nhktr92-deno-ts${scriptOrCompiled};${totalPasses};${durationInSec};1;algorithm=base,faithful=yes,bits=1`,
    );
  }

  if (verbose) {
    console.log(
      `\nThe first ${config["maxShowPrimes"]} found primes are:`,
      sieveResult["primeArray"],
    );
    console.log(
      `Passes: ${nrOfPasses}, Time: ${durationInSec.toFixed(2)},`,
      `Avg: ${(durationInSec / nrOfPasses).toFixed(8)} (sec/pass),`,
      `Sieve size: ${sieveSize}, Primes: ${
        sieveResult["countedPrimes"]
      }, Valid: ${validResult}`,
    );
  }
}

main();
