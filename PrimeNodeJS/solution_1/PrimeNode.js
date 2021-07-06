/*
NodeJS implementation of Prime Sieve
Based on:
previous implementation of NodeJS/solution_1 by Frank van Bakel
Python/solution_2 by ssovest
MyFirstPython Program (tm) Dave Plummer 8/9/2018

Author:    Rogier van Dam
Date:      2021-07-04
*/
'use strict';

var config = {
    'verbose'          : false,
    'timeLimitSeconds' : 5,
    'sieveSize'        : 1000000,
    'maxShowPrimes'    : 100,
};

// Historical data for validating our results - the number of primes
// to be found under some limit, such as 168 primes under 1000
var knownPrimeCounts = {
    10 : 4,
    100 : 25,
    1000 : 168,
    10000 : 1229,
    100000 : 9592,
    1000000 : 78498,
    10000000 : 664579,
    100000000 : 5761455
  }

// 32-bit bitarray for javascript, with only needed functions
class BitArray {

    constructor(size) {
        this.wordArray = new Uint32Array(1 + (size >> 5));              // allocation with 0
    }

    setBitTrue(index) {
        let wordOffset = index >> 5;                                    // 1 word = 2ˆ5 = 32 bit, so shift 5, much faster than /32
        let bitOffset  = index & 31;                                    // use & (and) for remainder, faster than modulus of /32
        this.wordArray[wordOffset] |= (1 << bitOffset);
    }

    testBitFalse(index) {
        let wordOffset = index >> 5;
        let bitOffset  = index & 31;
        let result = this.wordArray[wordOffset] & (1 << bitOffset);     // use a mask to only get the bit at position bitOffset.
        return (0 == result);
    }

}

/*
Main class for the prime calulation.
The BitArray stores only odd numbers, with formula number(index) = 2*index+1, e.g.
index = 0 -> number = 1
index = 1 -> number = 3
*/
class PrimeSieve {
    constructor(sievesize) {
        this.sievesize = sievesize;
        this.oddsize   = sievesize>>1;                                  // don't store even
        this.bitarray  = new BitArray(this.oddsize);
    }

    runSieve() {
        var q = Math.sqrt(this.oddsize);

        for ( var factor = 1; factor <= q ; factor++ ) {
            if (this.bitarray.testBitFalse(factor)) {
                let start = 2*factor*factor + 2*factor;
                let step  = factor * 2 + 1;

                for (var multiple = start ; multiple < this.oddsize ; multiple = multiple + step) {
                    this.bitarray.setBitTrue(multiple);                 // mask every multiple of this prime
                }
            }
        }
    }

    countPrimes() {
        var total = 0;
        for (var index=0; index < this.oddsize; index++) {
            if (this.bitarray.testBitFalse(index)) {                    // if bit is false, it's a prime, because true is masked
                total++;
            }
        }
        return total;
      }

    getPrimes(maxNr = 20) {
        let oddsize = this.oddsize;
        var primes = [2]; // this is a special prime
        var count = 1;
        if (this.oddsize > 1) {
            for (var factor=1; factor < oddsize; factor++ ) {
                if (count >= maxNr) break;
                if (this.bitarray.testBitFalse(factor)) {
                  count = primes.push( factor * 2 + 1 );
                }
            }
        }
        return primes;
    }

    validateResults() {
    }
}

function main(config) {
    let sieveSize = config['sieveSize'];

    // prepare timing
    var nrOfPasses = 0;                                                 // Counter for the number of passes in a from timestart to timefinish
    let timeStart = performance.now();                                  // Record starting time
    let timeFinish = timeStart + config['timeLimitSeconds'] * 1000;     // Calculate finish time before, so we don't repeat

    var sieve;                                                          // outside do loop to reference the last instance in verbose output
    do {
        sieve = new PrimeSieve(sieveSize);
        sieve.runSieve();
        nrOfPasses++;
    } while (performance.now() < timeFinish);                           // keep going for timeLimitSeconds

    // Mark end time
    let durationInMs  = performance.now() - timeStart;
    let durationInSec = durationInMs / 1000;

    // validate results
    var validResult = false;
    let countedPrimes = sieve.countPrimes();
    if (sieveSize in knownPrimeCounts) {
        let knownPrimeCount = knownPrimeCounts[sieveSize];
        validResult = (knownPrimeCount == countedPrimes);
        if (validResult) console.log(`\nrogiervandam;${nrOfPasses};${durationInSec};1;algorithm=base,faithful=yes,bits=1`);
        else             console.log(`\nError: invalid result. Limit for ${sieveSize} should be ${kknownPrimeCount} but result contains ${countedPrimes} primes`);
    }
    else console.log(`Warning: cannot validate result of ${countedPrimes} primes: limit ${sieveSize} is not in the known list of number of primes!`);

    if (config['verbose']) {
        if (config['maxShowPrimes']>0) {
          console.log(`\nThe first ${config['maxShowPrimes']} found primes are:`, sieve.getPrimes(config['maxShowPrimes']));
        }
        console.log(`Passes: ${nrOfPasses}, Time: ${parseFloat(durationInSec).toFixed(2)},`,
                    `Avg: ${parseFloat(durationInSec/nrOfPasses).toFixed(8)} (sec/pass),`,
                    `Sieve size: ${sieveSize}, Primes: ${countedPrimes}, Valid: ${validResult}`);
    }
}
main(config);
