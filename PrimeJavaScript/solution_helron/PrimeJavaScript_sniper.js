"use strict";
const { performance } = require('perf_hooks');
const NOW_UNITS_PER_SECOND = 1000;
let config = { sieveSize: 1000000, timeLimitSeconds: 5 };

class PrimeSniperAbsolute {
    constructor(sieveSize) {
        this.sieveSize = sieveSize;
        this.words = (sieveSize >>> 5) + 1;
        // ALLOCATION DU BUFFER EN INTERNE POUR ETRE 100% "FAITHFUL"
        // L'objet est instancié à zéro à chaque itération.
        this.pool = new ArrayBuffer(this.words * 4); // 4 bytes par 32-bit word
        this.arr = new Uint32Array(this.pool);
        this.arr.fill(0);
    }

    runSieve() {
        const arr = this.arr;
        const limitBits = this.sieveSize >>> 1;
        const q = Math.ceil(Math.sqrt(this.sieveSize)) >>> 1;

        let factor = 1;
        while (factor <= q) {
            if ((arr[factor >>> 5] & (1 << (factor & 31))) === 0) {
                const step = factor * 2 + 1;
                let start = factor * step + factor;

                const safeLimit = limitBits - (step << 3);
                while (start < safeLimit) {
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
            factor++;
        }
        return this;
    }

    countPrimes() {
        let count = 1;
        for (let i = 1; i < (this.sieveSize >>> 1); i++) {
            if ((this.arr[i >>> 5] & (1 << (i & 31))) === 0) count++;
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

    do {
        new PrimeSniperAbsolute(sieveSize).runSieve();
        nrOfPasses++;
    } while (performance.now() < timeFinish);

    const dur = (performance.now() - timeStart) / NOW_UNITS_PER_SECOND;
    console.log(`helron-sniper-node;${nrOfPasses};${dur};1;algorithm=base,faithful=yes,bits=1`);
}

if (!new PrimeSniperAbsolute(config.sieveSize).runSieve().validatePrimeCount()) {
    console.error("Échec de la validation mathématique !");
} else {
    runSieveBatch(config.sieveSize, config.timeLimitSeconds);
}
