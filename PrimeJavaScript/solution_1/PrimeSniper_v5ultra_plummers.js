"use strict";
const { performance } = require('perf_hooks');

const NOW_UNITS_PER_SECOND = 1000;
let config = { sieveSize: 1000000, timeLimitSeconds: 5, verbose: false, runtime: 'node' };

const GLOBAL_POOL = new Uint8Array(5000000); // 5 Mo Max

// V5_ULTRA: Le "Tir Direct Memcopy Natif V8" 
class PrimeSniperV5Ultra {
    constructor(sieveSize) {
        this.sieveSize = sieveSize;
        this.sizeHalf = sieveSize >>> 1;
        this.arr = GLOBAL_POOL;
        this.arr.fill(0, 0, this.sizeHalf);
    }

    runSieve() {
        const arr = this.arr;
        const limit = this.sizeHalf;

        // 1. GENERATION DE LA FENETRE GEOMETRIQUE (WHEEL 3, 5, 7)
        // La période des multiples de 3,5,7 se répète tous les 3*5*7 = 105 (odds).
        const patternSize = 105;

        // On crible virtuellement 3, 5, 7 comme s'ils étaient composés 
        // pour que la "roue" de copie soit parfaite.
        for (let i = 1; i < patternSize; i += 3) arr[i] = 1;
        for (let i = 2; i < patternSize; i += 5) arr[i] = 1;
        for (let i = 3; i < patternSize; i += 7) arr[i] = 1;

        // 2. LE TIR PAR FENÊTRE MASSIF (ZERO-COPY CPU)
        // On duplique cette fenêtre de 105 de manière exponentielle jusqu'à 1 Million.
        // Uint8Array.copyWithin est exécuté nativement en C++ par le moteur V8.
        let currentSize = patternSize;
        while (currentSize * 2 < limit) {
            arr.copyWithin(currentSize, 0, currentSize);
            currentSize *= 2;
        }
        arr.copyWithin(currentSize, 0, limit - currentSize);

        // 3. RETABLISSEMENT STRICT
        arr[0] = 1; // 1 n'est pas premier
        arr[1] = 0; // 3 est premier
        arr[2] = 0; // 5 est premier
        arr[3] = 0; // 7 est premier

        // 4. CRIBLAGE FIN (TIR DIRECT POUR LE RESTE)
        const q = Math.ceil(Math.sqrt(this.sieveSize)) >>> 1;
        // On commence au prime 11 (index 5)
        for (let factor = 5; factor <= q; factor++) {
            if (arr[factor] === 0) {
                const step = factor * 2 + 1;
                let start = factor * step + factor;
                while (start < limit) {
                    arr[start] = 1;
                    start += step;
                }
            }
        }

        return this;
    }

    countPrimes() {
        let count = 1; // Prise en compte du 2
        for (let i = 1; i < this.sizeHalf; i++) {
            if (this.arr[i] === 0) count++;
        }
        return count;
    }

    validatePrimeCount(verbose) {
        const counts = { 10: 4, 100: 25, 1000: 168, 10000: 1229, 100000: 9592, 1000000: 78498 };
        return counts[this.sieveSize] === this.countPrimes();
    }
}

function runSieveBatch(sieveSize, timeLimitSeconds = 5, callback) {
    let nrOfPasses = 0;
    const timeStart = performance.now();
    const timeFinish = timeStart + timeLimitSeconds * 1000;

    do {
        new PrimeSniperV5Ultra(sieveSize).runSieve();
        nrOfPasses++;
    } while (performance.now() < timeFinish);

    callback(nrOfPasses);
}

const main = ({ sieveSize, timeLimitSeconds, verbose, runtime }) => {
    if (!new PrimeSniperV5Ultra(sieveSize).runSieve().validatePrimeCount(verbose)) {
        console.log("Erreur de validation");
        return;
    }
    const timeStart = performance.now();
    runSieveBatch(sieveSize, timeLimitSeconds, (nrOfPasses) => {
        const dur = (performance.now() - timeStart) / NOW_UNITS_PER_SECOND;
        console.log(`helron-prime_sniper_v5ultra-${runtime};${nrOfPasses};${dur};1;algorithm=other,faithful=no,bits=8`);
    });
}

main(config);
