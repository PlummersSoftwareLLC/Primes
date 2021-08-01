/*
NodeJS implementation of Prime Sieve
Based on:
- previous implementation of NodeJS/solution_1 by Frank van Bakel
- Python/solution_2 by ssovest
- MyFirstPython Program (tm) Dave Plummer 8/9/2018

https://github.com/petkaantonov/bluebird/wiki/Optimization-killers
start with
node --trace-opt --trace-deopt --trace-ic PrimeNode_memcopy.js

Author:    Rogier van Dam
Date:      2021-07-18
*/
'use strict';

const { performance }                                  = require('perf_hooks'); // for compatibility with nodejs v14 stable (v16)

// 32-bit bitArray for javascript, with only needed functions
// int32, not uint and not 64bit because: javascript uses 32-bit int with bitwise operations
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Bitwise_AND
// shifting not with >> but with >>> is for zero fill right shift
class bitArray {
    constructor(size) {
        this.wordSize = 1 + (size >>> 5);
        let buffer = new Buffer.allocUnsafe(this.wordSize*4).fill(0);   // buffer with allocUnsafe slightly faster
        this.wordArray = new Int32Array(buffer);
    }

    setBitTrue(index) {
        const wordOffset = index >>> 5;                                 // 1 word = 2ˆ5 = 32 bit, so shift 5, much faster than /32
        const bitOffset  = index & 31;                                  // use & (and) for remainder, faster than modulus of /32
        this.wordArray[wordOffset] |= (1 << bitOffset);
    }

    testBitTrue(index) {
        const wordOffset = index >>> 5;
        const bitOffset  = index & 31;
        return this.wordArray[wordOffset] & (1 << bitOffset);           // returning result not as bool for performance
    }

    searchBitFalse(index) {
        while (this.testBitTrue(index)) { index++ };                    // will stop automatically because bits were 0 filled
        return index;
    }

    copyBlocks(blocksize_bits, range) {
        const blocksize_word = blocksize_bits >>> 5;                    // should be a multiple of 32, so no remainder
        const range_in_word = range >>> 5;

        for (let blockpart=0; blockpart < blocksize_word; blockpart++) {// reordered loops to increase cpu level1 cahce usage
            let block_word_offset = blocksize_word + blockpart;
            const copyOfPattern = this.wordArray[block_word_offset];
            while (block_word_offset < range_in_word) {
                block_word_offset += blocksize_word;
                this.wordArray[block_word_offset] = copyOfPattern;
            }
        }
    }

}

/*
Main class for the prime calulation.
The bitArray stores only odd numbers, with formula number(index) = 2*index+1, e.g.
index = 0 -> number = 1
index = 1 -> number = 3
*/
class PrimeSieve {
    constructor(sieveSize) {
        this.sieveSize = sieveSize;
        this.sieveSizeInBits = sieveSize >>> 1;
        this.bitArray  = new bitArray(1 + this.sieveSizeInBits);
    }

    runSieve() {
        let blocksize_bits = 3 << 5;                                    // a block is a repeating pattern of prime multiples, e.g. 3*5*7*32
        let range = blocksize_bits << 1;                                // range is the maximum to project the product of the prime
        let factor = 1;

        while (factor <= this.sieveSizeInBits) {
            const start = factor * factor * 2 + factor + factor;
            const step  = factor * 2 + 1;
            if (start > range) break;                                   // stopping here is cheaper than calculating square root of factor as start

            for (let product = start; product < range; product += step) {
                this.bitArray.setBitTrue(product);                      // mark every multiple of this prime
            }

            factor = this.bitArray.searchBitFalse(factor+1);

            if (range < this.sieveSizeInBits) {                         // check if we should copy previous results
                const blocksize_bits_new = blocksize_bits * (factor*2+1);
                range = blocksize_bits_new << 1;                        // range is x2 so the second block cointains all multiples of primes
                if ((range << 1) > this.sieveSizeInBits) {                       // only use this copy method if it is useful (2+2-1=3 copies or more)
                    range = this.sieveSizeInBits;
                }

                this.bitArray.copyBlocks(blocksize_bits, range);
                blocksize_bits = blocksize_bits_new;
            }
        }
    }

    countPrimes() {
        let primeCount = 1;                                             // account for prime 2
        for (let index = 1; index < this.sieveSizeInBits; index++ ) {
            if (!this.bitArray.testBitTrue(index)) {                    // if bit is false, it's a prime, because non-primes are marked true
                primeCount++;
            }
        }
        return primeCount;
      }

    getPrimes(maxNr = 100) {
        let primeArray = [2];                                           // 2 is a special prime
        let count = 1;
        if (this.sieveSize > 1) {
            for (let factor = 1; factor < this.sieveSizeInBits; factor++ ) {
                if (count >= maxNr) break;
                if (!this.bitArray.testBitTrue(factor)) {
                  count = primeArray.push(factor * 2 + 1);
                }
            }
        }
        return primeArray;
    }

    deepAnalyzePrimes() {
        let sieve = this;
        console.log("DeepAnalyzing");
        let range_to =  sieve.sieveSizeInBits;
        let warn_prime = 0;
        let warn_nonprime = 0;
        for (let factor = 1; factor < range_to; factor++ ) {
            if (!this.bitArray.testBitTrue(factor)) {                   // is this a prime?
                let q = (Math.sqrt(factor*2+1)|1)*2+1;
                for(let c=1; c<=q; c++) {
                    if ((factor*2+1) % (c*2+1) == 0 && (c*2+1) != (factor*2+1)) {
                        if (warn_prime++ < 30) {
                            console.log("Number %d (%d) was marked prime, but %d * %d = %d", factor*2+1, factor,  c*2+1, (factor*2+1)/(c*2+1), factor*2+1 );
                        }
                    }
                }
            }
            else {
                let q = (Math.sqrt(factor*2+1)|1)*2+1;
                let c_factor = 0;
                for(let c=1; c<=q; c++) {
                    if ((factor*2+1) % (c*2+1) == 0 && (c*2+1) != (factor*2+1)) {
                        c_factor++;
                        break;
                    }
                }
                if (c_factor==0) {
                    if (warn_nonprime++ < 30) {
                        console.log("Number %d (%d) was marked non-prime, but no factors found. So it is prime", factor*2+1, factor);
                    }
                }
            }
        }
    }

}

// run the sieve for timeLimitSeconds
function runSieveBatch(sieveSize, timeLimitSeconds=5) {
    let totalPasses = 0;

    const timeStart  = performance.now();
    const timeFinish = timeStart + timeLimitSeconds * 1000;             // Calculate finish time before, so we don't repeat this calculation

    let sieve;                                                          // outside do loop to reference the last instance in verbose output
    while (performance.now() < timeFinish) {
        sieve = new PrimeSieve(sieveSize);
        sieve.runSieve();
        totalPasses++;
    }
    let timeEnd = performance.now();                                    // measure one final time, just to be fair
    return {
        "durationInSec" : (timeEnd - timeStart) / 1000,
        "totalPasses"   : totalPasses
    }
}

// get a single sieve (for validation and statistics)
function validateSieveAlgoritm(sieveSize, maxShowPrimes=100) {

    // Historical data for validating our results - the number of primes
    // to be found under some limit, such as 168 primes under 1000
    const knownPrimeCounts = {
        10 : 4,
        100 : 25,
        1000 : 168,
        10000 : 1229,
        100000 : 9592,
        1000000 : 78498,
        10000000 : 664579,
        100000000 : 5761455
      }

    let sieve         = new PrimeSieve(sieveSize);
    sieve.runSieve();
    let countedPrimes = sieve.countPrimes();
    let primeArray    = sieve.getPrimes(maxShowPrimes);

    let validResult = false, validationMessage = "";
    if (sieveSize in knownPrimeCounts) {
        validResult = (knownPrimeCounts[sieveSize] == countedPrimes);
        if (!validResult) {
            validationMessage = `Error: invalid result. A sieve with limit ${sieveSize} should have ${knownPrimeCounts[sieveSize]} primes but result contains ${countedPrimes} primes`;
        }
    }
    else {
        validationMessage = `Warning: cannot validate result of ${countedPrimes} primes: The prime count for a sieve with limit ${sieveSize} is unknown!`;
        validResult = undefined;
    }

    return {
        "countedPrimes"     : countedPrimes,
        "primeArray"        : primeArray,
        "validResult"       : validResult,
        "validationMessage" : validationMessage,
        "sieve"             : sieve                                     // needed for deepanalyze
    }
}

/*
main procedure
*/
function main() {
    let sieveSize        = 1000000;
    let timeLimitSeconds = 5;
    let verbose          = false;
    let maxShowPrimes    = verbose ? 100 : 0;

    // validate algorithm - run once to check if valid - no need for benchmark if invalid
    let validationResult = validateSieveAlgoritm(sieveSize);

    if (validationResult["validResult"] === true) {
        //measure time running the batch
        let batchResult   = runSieveBatch(sieveSize, timeLimitSeconds);
        let totalPasses   = batchResult['totalPasses'];
        let durationInSec = batchResult['durationInSec'];

        //output for benchmarking purposes
        console.log(`\nrogiervandam_memcopy;${totalPasses};${durationInSec};1;algorithm=other,faithful=yes,bits=1`);

        if (verbose) {
            console.log(require("os").cpus()[0].model);
            console.log(`Passes: ${batchResult['totalPasses']}, Time: ${parseFloat(batchResult['durationInSec']).toFixed(2)},`,
                        `Avg: ${parseFloat(batchResult['durationInSec']/batchResult['totalPasses']).toFixed(8)} (sec/pass),`,
                        `${parseFloat(batchResult['totalPasses']/batchResult['durationInSec']).toFixed(8)} (pass/sec),`,
                        `Sieve size: ${sieveSize}, Primes: ${validationResult['countedPrimes']}, Valid: ${validationResult['validResult']}`);
        }
    }
    else {
        // output for validation purposes
        console.log(validationResult["validationMessage"]);
        if (validationResult["validResult"] === false) {
            console.log(`\nThe first ${maxShowPrimes} found primes are:`, validationResult['primeArray']);
            validationResult["sieve"].deepAnalyzePrimes();
        }
    }
}
main();
