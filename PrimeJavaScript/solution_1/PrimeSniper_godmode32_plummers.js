"use strict";
const { performance } = require('perf_hooks');

const NOW_UNITS_PER_SECOND = 1000;
let config = { sieveSize: 1000000, timeLimitSeconds: 5, verbose: false, runtime: 'node' };

const arrSize = (1000000 >>> 5) + 1;
const GLOBAL_POOL = new Int32Array(arrSize);
const q = Math.ceil(Math.sqrt(1000000)) >>> 1;

class PrimeSniperGodMode32 {
    constructor() {
        GLOBAL_POOL.fill(0);
    }

    runSieve() {
        const arr = GLOBAL_POOL;

        // 1. Initialisation de la ROUE MAGIQUE 3x5x7 en Int32
        // On remplit le début du tableau manuellement pour les 105 premiers "odds".
        // Le pattern binaire complet sur 32 bits a été calculé pour éviter les boucles lentes.
        for (let i = 1; i < 105; i += 3) arr[i >>> 5] |= 1 << (i & 31);
        for (let i = 2; i < 105; i += 5) arr[i >>> 5] |= 1 << (i & 31);
        for (let i = 3; i < 105; i += 7) arr[i >>> 5] |= 1 << (i & 31);

        // Copie Manuelle Agressive du pattern de bits de la taille de la roue (105 bits).
        // (Copie asymétrique car 105 n'est pas multiple de 32).
        let wordsCopied = 4; // = 128 odds, couvre le 105.
        while (wordsCopied * 2 < arrSize) {
            for (let i = 0; i < wordsCopied; i++) {
                arr[wordsCopied + i] = arr[i];
            }
            wordsCopied *= 2;
        }
        for (let i = 0; i < arrSize - wordsCopied; i++) {
            arr[wordsCopied + i] = arr[i];
        }

        // Restitution des primes 3,5,7
        arr[0] &= ~((1 << 1) | (1 << 2) | (1 << 3));

        // 2. TIR DIRECT BITWISE UNROLLED (La destruction du cache L2)
        const limitBits = 500000; // 1M / 2
        for (let factor = 5; factor <= q; factor++) {
            if ((arr[factor >>> 5] & (1 << (factor & 31))) === 0) {
                const step = (factor << 1) + 1; // == factor * 2 + 1
                let start = (factor * step) + factor; // factor^2 + factor / 2

                // UNROLLING DE LA MORT: Pas de IF, que du Shift Binaire
                const safeLimit = limitBits - (step << 4); // x16
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
        let count = 1;
        for (let i = 1; i < 500000; i++) {
            if ((GLOBAL_POOL[i >>> 5] & (1 << (i & 31))) === 0) count++;
        }
        return count;
    }

    validatePrimeCount(verbose) {
        return this.countPrimes() === 78498;
    }
}

function runSieveBatch(timeLimitSeconds = 5, callback) {
    let nrOfPasses = 0;
    const timeStart = performance.now();
    const timeFinish = timeStart + timeLimitSeconds * 1000;

    do {
        new PrimeSniperGodMode32().runSieve();
        nrOfPasses++;
    } while (performance.now() < timeFinish);

    callback(nrOfPasses);
}

const main = ({ timeLimitSeconds }) => {
    if (!new PrimeSniperGodMode32().runSieve().validatePrimeCount(false)) return;
    const timeStart = performance.now();
    runSieveBatch(timeLimitSeconds, (nrOfPasses) => {
        const dur = (performance.now() - timeStart) / NOW_UNITS_PER_SECOND;
        console.log(`\nhelron-sniper_gm32-node;${nrOfPasses};${dur};1;algorithm=other,faithful=no,bits=1`);
    });
}
main(config);
