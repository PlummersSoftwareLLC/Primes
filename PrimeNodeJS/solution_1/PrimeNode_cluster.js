/*
NodeJS implementation of Prime Sieve
Based on:
previous implementation of NodeJS/solution_1 by Frank van Bakel
Python/solution_2 by ssovest
MyFirstPython Program (tm) Dave Plummer 8/9/2018

Author:    Rogier van Dam
Date:      2021-07-08
*/
'use strict';

// get performance API (for older node versions) and cluster api for multiprocessor
const { performance } = require('perf_hooks');
const cluster         = require('cluster');
const os              = require('os');

let config = {
    'verbose'           : false,
    'timeLimitSeconds'  : 5,
    'sieveSize'         : 1000000,
    'maxShowPrimes'     : 100,

    // automatically get # of threads from os, if intel assume hyperthreading
    'workers'           : os.cpus().length / (os.cpus()[0].model.includes('Intel')?2:1)
};

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

// 32-bit bitarray for javascript, with only needed functions
// int32, not uint and not 64bit because: javascript uses 32int
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Bitwise_AND
// shifting not with >> but with >>> is for zero fill right shift
class BitArray {
    constructor(size) {
        this.wordArray = new Int32Array(1 + (size >>> 5));              // allocation in javascript is with 0
    }

    setBitTrue(index) {
        const wordOffset = index >>> 5;                                 // 1 word = 2ˆ5 = 32 bit, so shift 5, much faster than /32
        const bitOffset  = index & 31;                                  // use & (and) for remainder, faster than modulus of /32
        this.wordArray[wordOffset] |= (1 << bitOffset);
    }

    testBitTrue(index) {
        const wordOffset = index >>> 5;
        const bitOffset  = index & 31;
        return this.wordArray[wordOffset] & (1 << bitOffset);           // use a mask to only get the bit at position bitOffset. >0=true, 0=false
    }

}

/*
Main class for the prime calulation.
The BitArray stores only odd numbers, with formula number(index) = 2*index+1, e.g.
index = 0 -> number = 1
index = 1 -> number = 3
*/
class PrimeSieve {
    constructor(sieveSize) {
        this.sieveSize = sieveSize;
        this.oddsize = sieveSize >>> 1;
        this.bitarray  = new BitArray(1+this.oddsize);
    }

    runSieve() {
        const q = Math.ceil( Math.sqrt(this.oddsize) );                 // convert to integer with ceil

        for (let factor = 1; factor <= q ; factor++ ) {
            if (!this.bitarray.testBitTrue(factor)) {
                const step  = factor * 2 + 1 ;
                const start = factor * factor * 2 + factor + factor;

                for (let multiple = start ; multiple < this.oddsize ; multiple = multiple + step) {
                    this.bitarray.setBitTrue(multiple);                 // mark every multiple of this prime
                }
            }
        }
    }

    countPrimes() {
        let total = 1;                                                  // account for prime 2
        for (let index = 1; index < this.oddsize; index++ ) {
            if (!this.bitarray.testBitTrue(index)) {                    // if bit is false, it's a prime, because non-primes are marked true
                total++;
            }
        }
        return total;
      }

    getPrimes(max = 100) {
        let primes = [2];                                               // 2 is a special prime
        let count = 0;
        for (let factor = 1; factor < this.oddsize; factor++ ) {
            if (count >= max) break;
            if (!this.bitarray.testBitTrue(factor)) {
              count = primes.push( factor * 2 + 1 );
            }
        }
        return primes;
    }

}

// run the sieve for timeLimitSeconds
function runSieveBatch(sieveSize, timeLimitSeconds=5) {
    let nrOfPasses = 0;                                                 // Counter for the number of passes in a from timestart to timefinish

    const timeStart = performance.now();                                // Record starting time
    const timeFinish = timeStart + timeLimitSeconds * 1000;             // Calculate finish time before, so we don't repeat

    let sieve;                                                          // outside do loop to reference the last instance in verbose output
    do {
        sieve = new PrimeSieve(sieveSize);
        sieve.runSieve();
        nrOfPasses++;
    } while (performance.now() < timeFinish);                           // keep going for timeLimitSeconds

    return nrOfPasses;
}

// get a single sieve (for validation and statistics)
function evalSieve(sieveSize, maxShowPrimes=100) {
    let sieve = new PrimeSieve(sieveSize);
    sieve.runSieve();
    return {
        "countedPrimes": sieve.countPrimes(),
        "primeArray"   : sieve.getPrimes(maxShowPrimes)
    }
}

function printResult(sieveSize, nrOfPasses=0, durationInSec=0, verbose ) {

    // validate algorithm - run one final time on the result
    let sieveResult = evalSieve(sieveSize, config['maxShowPrimes']);
    let validResult = false;
    if (sieveSize in knownPrimeCounts) {
        let knownPrimeCount = knownPrimeCounts[sieveSize];
        validResult = (knownPrimeCount == sieveResult['countedPrimes']);
        if (!validResult) console.log(`\nError: invalid result. Limit for ${sieveSize} should be ${knownPrimeCount} but result contains ${sieveResult['countedPrimes']} primes`);
    }
    else console.log(`Warning: cannot validate result of ${sieveResult['countedPrimes']} primes: limit ${sieveSize} is not in the known list of number of primes!`);

    if (validResult) {
        console.log(`\nrogiervandam;${nrOfPasses};${durationInSec};${config['workers']};algorithm=base,faithful=yes,bits=1`);
    }

    if (config['verbose']) {
        console.log(`\nThe first ${config['maxShowPrimes']} found primes are:`, sieveResult['primeArray']);
        console.log(`Passes: ${nrOfPasses}, Time: ${parseFloat(durationInSec).toFixed(2)},`,
                    `Avg: ${parseFloat(durationInSec/nrOfPasses).toFixed(8)} (sec/pass), ${parseFloat(nrOfPasses/durationInSec).toFixed(2)} (pass/sec)`,
                    `Sieve size: ${sieveSize}, Primes: ${sieveResult['countedPrimes']}, Valid: ${validResult}`);
    }

}


function main(sieveSize, timeLimitSeconds=5, maxWorkers=1) {
    let totalPasses = 0;

    // fire off the helpers
    let timeStart = performance.now();
    for (let i=0; i < maxWorkers; i++) {
        const worker = cluster.fork();
        worker.send({ workerData: {
            "sieveSize"         : sieveSize,
            "timeLimitSeconds"  : timeLimitSeconds
        }});
        worker.on('message', result => {
            totalPasses += result.nrOfPasses;
        })
    }

    cluster.on('exit', (worker, code, signal) => {
        let countRunning = Object.keys(cluster.workers).length;
        if (countRunning == 0) {
            let timeEnd       = performance.now();                      // time the last worker, not the first (to give fair results)
            let durationInMs  = timeEnd - timeStart;
            let durationInSec = durationInMs / 1000;
            printResult(sieveSize, totalPasses, durationInSec);
        }
    });

}

/*
main procedure
*/

if (cluster.isMaster) {
    main(   config['sieveSize'],
            config['timeLimitSeconds'],
            config['workers']
    );
}

if (cluster.isWorker) { // is worker for multithreaded
    process.on('message', msg => {
        let workerData = msg['workerData'];
        let sieveSize        = workerData['sieveSize'];
        let timeLimitSeconds = workerData["timeLimitSeconds"];
        let nrOfPasses       = runSieveBatch(sieveSize, timeLimitSeconds);

        process.send({ "nrOfPasses": nrOfPasses, "durationInSec": timeLimitSeconds });
        process.exit(0);
    });
}
