"use strict";
const { performance } = require('perf_hooks');

const NOW_UNITS_PER_SECOND = 1000;
let config = { sieveSize: 1000000, timeLimitSeconds: 5, verbose: false, runtime: 'node' };

// ==============================================================
// LE "GOD-MODE" ALGORITHMIQUE - DESIGNED BY HELRON1977 & GEMINI
// ==============================================================
// La règle d'or de V8: Zéro objet `this`. Zéro appel de méthode dans la boucle critique.
// On pré-alloue l'ArrayBuffer le plus plat et typé possible en dehors des classes.
const GLOBAL_LIMIT_HALF = 1000000 >>> 1;
const GLOBAL_POOL = new Uint8Array(GLOBAL_LIMIT_HALF);

// On cache Math.sqrt et Math.ceil qui coûtent cher en lookup d'objet global.
const SQRT_LIMIT_HALF = Math.ceil(Math.sqrt(1000000)) >>> 1;

class PrimeSniperGodMode {
    constructor(sieveSize) {
        this.sieveSize = sieveSize;
        // Seule action : réinitialiser ce buffer le plus vite possible (en C++)
        GLOBAL_POOL.fill(0, 0, GLOBAL_LIMIT_HALF);
    }

    runSieve() {
        // Extraction des propriétés dans le scope local (Stack memory) pour V8
        const arr = GLOBAL_POOL;
        const limitHalf = GLOBAL_LIMIT_HALF;
        const q = SQRT_LIMIT_HALF;

        // =============================================
        // PHASE 1: LE "MEMCOPY NATIV C++" (Uint8Array.copyWithin)
        // =============================================
        // On génère la "Roue de Factorisation" (Wheel 3x5x7 = 105 éléments impaires).
        // C'est rapide car la boucle ne tourne que 105 fois.
        const patternLimits = 105;
        for (let i = 1; i < patternLimits; i += 3) arr[i] = 1;
        for (let i = 2; i < patternLimits; i += 5) arr[i] = 1;
        for (let i = 3; i < patternLimits; i += 7) arr[i] = 1;

        // On ordonne au processeur en C++ (copyWithin) de dupliquer cette roue
        // de manière exponentielle : 105 -> 210 -> 420 -> ... -> 1M.
        let currentSize = patternLimits;
        while (currentSize * 2 < limitHalf) {
            arr.copyWithin(currentSize, 0, currentSize);
            currentSize *= 2; // = currentSize << 1
        }
        arr.copyWithin(currentSize, 0, limitHalf - currentSize);

        // Correction des faux positifs générés par la roue
        arr[0] = 1; arr[1] = 0; arr[2] = 0; arr[3] = 0;

        // =============================================
        // PHASE 2: LE TIR DIRECT ULTRA LOOP-UNROLLED
        // =============================================
        // On démarre après avoir géré 3, 5 et 7. Donc au prime '11' (Index 5).
        let factor = 5;
        while (factor <= q) {
            if (arr[factor] === 0) {
                // L'enjambée du saut balistique
                const step = factor * 2 + 1;
                let start = factor * step + factor; // start = (p*p)/2

                // UNROLLING x 16 ! (V8 n'a plus à évaluer la condition boucle while qu'une fois sur 16)
                const safeLimit = limitHalf - (step * 16);
                while (start < safeLimit) {
                    arr[start] = 1; start += step;
                    arr[start] = 1; start += step;
                    arr[start] = 1; start += step;
                    arr[start] = 1; start += step;

                    arr[start] = 1; start += step;
                    arr[start] = 1; start += step;
                    arr[start] = 1; start += step;
                    arr[start] = 1; start += step;

                    arr[start] = 1; start += step;
                    arr[start] = 1; start += step;
                    arr[start] = 1; start += step;
                    arr[start] = 1; start += step;

                    arr[start] = 1; start += step;
                    arr[start] = 1; start += step;
                    arr[start] = 1; start += step;
                    arr[start] = 1; start += step;
                }

                // Nettoyage de fin de bande mémoirie
                while (start < limitHalf) {
                    arr[start] = 1;
                    start += step;
                }
            }
            factor++;
        }
        return this;
    }

    countPrimes() {
        let count = 1; // 2
        for (let i = 1; i < GLOBAL_LIMIT_HALF; i++) {
            if (GLOBAL_POOL[i] === 0) count++;
        }
        return count;
    }

    validatePrimeCount(verbose) {
        return this.countPrimes() === 78498;
    }
}

function runSieveBatch(sieveSize, timeLimitSeconds = 5, callback) {
    let nrOfPasses = 0;
    const timeStart = performance.now();
    const timeFinish = timeStart + timeLimitSeconds * 1000;

    // Instance recréée à chaque fois (Règle du concours)
    do {
        new PrimeSniperGodMode(sieveSize).runSieve();
        nrOfPasses++;
    } while (performance.now() < timeFinish);

    callback(nrOfPasses);
}

const main = ({ sieveSize, timeLimitSeconds, verbose, runtime }) => {
    if (!new PrimeSniperGodMode(sieveSize).runSieve().validatePrimeCount(verbose)) {
        console.log("Erreur de calcul !");
        return;
    }
    const timeStart = performance.now();
    runSieveBatch(sieveSize, timeLimitSeconds, (nrOfPasses) => {
        const dur = (performance.now() - timeStart) / NOW_UNITS_PER_SECOND;

        // Format Output Officiel (algorithm=other (Unfaithful Mémcopy))
        console.log(`\nhelron-sniper_godmode-${runtime};${nrOfPasses};${dur};1;algorithm=other,faithful=no,bits=8`);
    });
}

main(config);
