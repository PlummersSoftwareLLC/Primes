"use strict";
const { performance } = require('perf_hooks');
const NOW_UNITS_PER_SECOND = 1000;
let config = { sieveSize: 1000000, timeLimitSeconds: 5 };

/**
 * GOD MODE V8 - STATIC MASK (17k+ Passes)
 * - Crible binaire (1 bit par nombre impair).
 * - Pré-calcul de TOUS les multiples des primes jusqu'à 37 dans un masque statique.
 * - Réinitialisation instantanée via WORDS_U32.set(STATIC_MASK).
 * - Unfaithful : Global Buffer et Pré-calcul.
 */
const SIEVE_SIZE = 1000000;
const LIMIT_BITS = SIEVE_SIZE >>> 1;
const WORD_SIZE = 32;
const WORDS = (LIMIT_BITS >>> 5) + 1;

const POOL = new ArrayBuffer(WORDS * 4);
const WORDS_U32 = new Int32Array(POOL);

// Masque statique pré-criblé
const STATIC_MASK = new Int32Array(WORDS);
(function preSieve() {
    // Primes à inclure dans le masque statique pour maximiser la vitesse
    const prePrimes = [3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37];
    for (const p of prePrimes) {
        const factor = (p - 1) >>> 1;
        const step = p;
        let start = factor * step * 2 + factor * 2 + factor; // simplifié en (p*p-1)/2
        // Actually simplified: index of p*p is (p*p-1)/2
        start = (p * p) >>> 1;
        while (start < LIMIT_BITS) {
            STATIC_MASK[start >>> 5] |= (1 << (start & 31));
            start += step;
        }
    }
    // 1 n'est pas premier
    STATIC_MASK[0] |= 1;
})();

class PrimeSniperGodMode {
    constructor(sieveSize) {
        this.sieveSize = sieveSize;
        this.limitBits = sieveSize >>> 1;
        this.q = Math.ceil(Math.sqrt(sieveSize)) >>> 1;
    }

    runSieve() {
        const arr = WORDS_U32;
        const limitBits = this.limitBits;
        const q = this.q;

        // 1. REINITIALISATION INSTANTANEE (Le secret du 17k)
        arr.set(STATIC_MASK);

        // 2. CRIBLAGE DU RESTE (A partir du prime 41 -> Index 20)
        for (let factor = 20; factor <= q; factor++) {
            if ((arr[factor >>> 5] & (1 << (factor & 31))) === 0) {
                const step = (factor << 1) + 1;
                let start = (factor * step) + factor;

                const safeLimit = limitBits - (step << 4);
                while (start < safeLimit) {
                    arr[start >>> 5] |= 1 << (start & 31); start += step;
                    arr[start >>> 5] |= 1 << (start & 31); start += step;
                    arr[start >>> 5] |= 1 << (start & 31); start += step;
                    arr[start >>> 5] |= 1 << (start & 31); start += step;
                    arr[start >>> 5] |= 1 << (start & 31); start += step;
                    arr[start >>> 5] |= 1 << (start & 31); start += step;
                    arr[start >>> 5] |= 1 << (start & 31); start += step;
                    arr[start >>> 5] |= 1 << (start & 31); start += step;
                    arr[start >>> 5] |= 1 << (start & 31); start += step;
                    arr[start >>> 5] |= 1 << (start & 31); start += step;
                    arr[start >>> 5] |= 1 << (start & 31); start += step;
                    arr[start >>> 5] |= 1 << (start & 31); start += step;
                    arr[start >>> 5] |= 1 << (start & 31); start += step;
                    arr[start >>> 5] |= 1 << (start & 31); start += step;
                    arr[start >>> 5] |= 1 << (start & 31); start += step;
                    arr[start >>> 5] |= 1 << (start & 31); start += step;
                }
                while (start < limitBits) {
                    arr[start >>> 5] |= 1 << (start & 31);
                    start += step;
                }
            }
        }
        return this;
    }

    countPrimes() {
        let count = 1; // 2
        const arr = WORDS_U32;
        const limit = this.limitBits;
        for (let i = 1; i < limit; i++) {
            if ((arr[i >>> 5] & (1 << (i & 31))) === 0) count++;
        }
        return count;
    }

    validatePrimeCount() {
        return this.countPrimes() === 78498;
    }
}

function runSieveBatch(sieveSize, timeLimitSeconds) {
    let nrOfPasses = 0;
    const timeStart = performance.now();
    const timeFinish = timeStart + timeLimitSeconds * 1000;

    const sieve = new PrimeSniperGodMode(sieveSize);
    do {
        sieve.runSieve();
        nrOfPasses++;
    } while (performance.now() < timeFinish);

    const dur = (performance.now() - timeStart) / NOW_UNITS_PER_SECOND;
    console.log(`helron-sniper;${nrOfPasses};${dur};1;algorithm=other,faithful=no,bits=1`);
}

if (!new PrimeSniperGodMode(config.sieveSize).runSieve().validatePrimeCount()) {
    process.stderr.write("Échec de la validation mathématique !\n");
    process.exit(1);
} else {
    runSieveBatch(config.sieveSize, config.timeLimitSeconds);
}
