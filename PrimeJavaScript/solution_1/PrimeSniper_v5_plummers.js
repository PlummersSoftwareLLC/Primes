"use strict";
const { performance } = require('perf_hooks');

const NOW_UNITS_PER_SECOND = 1000;
let config = { sieveSize: 1000000, timeLimitSeconds: 5, verbose: false, runtime: 'node' };

// V5 - "Tir Direct Bitwise Extrême" (Unfaithful/Other, Bits=1)
// On recycle un pointeur mémoire pour éviter le Garbage Collector (Autorisé en 'Other')
const GLOBAL_MEMORY_POOL = new Int32Array(500000); // Max ~32M candidats

class PrimeSniperV5 {
    constructor(sieveSize) {
        this.sieveSize = sieveSize;
        this.sizeBits = sieveSize >>> 1;
        this.wordArray = GLOBAL_MEMORY_POOL;
        // Nettoyage ultra rapide du pool pour ce passage
        this.wordArray.fill(0, 0, 1 + (this.sizeBits >>> 5));
    }

    runSieve() {
        const limitBits = this.sizeBits;
        const arr = this.wordArray;
        const q = Math.ceil(Math.sqrt(this.sieveSize)) >>> 1;

        // Cœur de l'approche : Elimination ciblée sans division
        let factor = 1; // Index 1 => Nombre 3
        while (factor <= q) {
            // Test de primalité bitwise : 1 opération
            if ((arr[factor >>> 5] & (1 << (factor & 31))) === 0) {
                const step = factor * 2 + 1;
                let start = factor * step + factor; // equivalent to (p*p) >>> 1

                // Le Tir Direct : Déroulement de boucle massif par 8 (Réduction des cycles CPU)
                const safeLimit = limitBits - (step << 3);
                while (start < safeLimit) {
                    arr[start >>> 5] |= (1 << (start & 31)); start += step;
                    arr[start >>> 5] |= (1 << (start & 31)); start += step;
                    arr[start >>> 5] |= (1 << (start & 31)); start += step;
                    arr[start >>> 5] |= (1 << (start & 31)); start += step;

                    arr[start >>> 5] |= (1 << (start & 31)); start += step;
                    arr[start >>> 5] |= (1 << (start & 31)); start += step;
                    arr[start >>> 5] |= (1 << (start & 31)); start += step;
                    arr[start >>> 5] |= (1 << (start & 31)); start += step;
                }
                // Finition du tir
                while (start < limitBits) {
                    arr[start >>> 5] |= (1 << (start & 31));
                    start += step;
                }
            }
            factor++;
        }
        return this;
    }

    countPrimes() {
        let count = 1; // 2
        for (let i = 1; i < this.sizeBits; i++) {
            if ((this.wordArray[i >>> 5] & (1 << (i & 31))) === 0) {
                count++;
            }
        }
        return count;
    }

    validatePrimeCount(verbose) {
        const counts = { 10: 4, 100: 25, 1000: 168, 10000: 1229, 100000: 9592, 1000000: 78498, 10000000: 664579 };
        return counts[this.sieveSize] === this.countPrimes();
    }
}

function runSieveBatch(sieveSize, timeLimitSeconds = 5, callback) {
    let nrOfPasses = 0;
    const timeStart = performance.now();
    const timeFinish = timeStart + timeLimitSeconds * 1000;

    do {
        new PrimeSniperV5(sieveSize).runSieve();
        nrOfPasses++;
    } while (performance.now() < timeFinish);

    callback(nrOfPasses);
}

const main = ({ sieveSize, timeLimitSeconds, verbose, runtime }) => {
    if (!new PrimeSniperV5(sieveSize).runSieve().validatePrimeCount(verbose)) return;
    const timeStart = performance.now();
    runSieveBatch(sieveSize, timeLimitSeconds, (nrOfPasses) => {
        const dur = (performance.now() - timeStart) / NOW_UNITS_PER_SECOND;
        console.log(`helron-prime_sniper_v5-${runtime};${nrOfPasses};${dur};1;algorithm=other,faithful=no,bits=1`);
    });
}

main(config);
