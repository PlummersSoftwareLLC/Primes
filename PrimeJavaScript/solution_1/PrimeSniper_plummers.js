"use strict";
const { performance } = require('perf_hooks');
const { getPrimesV4 } = require('../../../../index');

const NOW_UNITS_PER_SECOND = 1000;

let config = {
    sieveSize: 1000000,
    timeLimitSeconds: 5,
    verbose: false,
    runtime: 'node'
};

// ===============================================
// CLASSE COMPÉTITIVE POUR PRIME-SNIPER V4
// ===============================================
class PrimeSniperSieve {
    constructor(sieveSize) {
        this.sieveSize = sieveSize;
        this.primes = null;
    }

    runSieve() {
        // Exécution de l'algorithme pur (V4)
        this.primes = getPrimesV4(this.sieveSize);
        return this;
    }

    countPrimes() {
        return this.primes.length;
    }

    getPrimes(maxNr = 100) {
        return Array.from(this.primes.slice(0, maxNr));
    }

    validatePrimeCount(verbose) {
        const knownPrimeCounts = {
            10: 4,
            100: 25,
            1000: 168,
            10000: 1229,
            100000: 9592,
            1000000: 78498,
            10000000: 664579,
            100000000: 5761455
        };

        const countedPrimes = this.countPrimes();
        let validResult = false;

        if (this.sieveSize in knownPrimeCounts) {
            const knownPrimeCount = knownPrimeCounts[this.sieveSize];
            validResult = (knownPrimeCount == countedPrimes);
            if (!validResult) {
                console.log(`Error: Result contains ${countedPrimes} primes instead of ${knownPrimeCount}`);
            }
        }
        return validResult;
    }
}

// ===============================================
// BOUCLE DE BENCHMARK OFFICIELLE PLUMMER
// ===============================================
function runSieveBatch(sieveSize, timeLimitSeconds = 5, callback) {
    let nrOfPasses = 0;

    const timeStart = performance.now();
    const timeFinish = timeStart + timeLimitSeconds * 1000;

    let sieve;
    do {
        sieve = new PrimeSniperSieve(sieveSize);
        sieve.runSieve();
        nrOfPasses++;
    } while (performance.now() < timeFinish);

    callback(nrOfPasses);
}

const main = ({ sieveSize, timeLimitSeconds, verbose, runtime }) => {
    // 1. Validation de la conformité (très strict chez Plummer)
    const validResult = new PrimeSniperSieve(sieveSize).runSieve().validatePrimeCount(verbose);
    if (!validResult) return false;

    // 2. Mesure du temps
    const timeStart = performance.now();
    runSieveBatch(sieveSize, timeLimitSeconds, (nrOfPasses) => {
        const timeEnd = performance.now();
        const durationInSec = (timeEnd - timeStart) / NOW_UNITS_PER_SECOND;

        // Output Officiel format Plummer
        console.log(`\nhelron-prime_sniper_v4-${runtime};${nrOfPasses};${durationInSec};1;algorithm=other,faithful=no,bits=8`);
    });
}

main(config);
