"use strict";
const { performance } = require('perf_hooks');

const NOW_UNITS_PER_SECOND = 1000;
let config = { sieveSize: 1000000, timeLimitSeconds: 5, verbose: false, runtime: 'node' };

const GLOBAL_POOL = new Uint8Array(500000);

class PrimeSniperV5b {
    constructor(sieveSize) {
        this.sieveSize = sieveSize;
        this.sizeHalf = Math.floor(sieveSize / 2);
        this.arr = GLOBAL_POOL;
        this.arr.fill(0, 0, this.sizeHalf);
    }

    runSieve() {
        const arr = this.arr;
        const sizeHalf = this.sizeHalf;
        const q = Math.ceil(Math.sqrt(this.sieveSize)) >>> 1;

        for (let factor = 1; factor <= q; factor++) {
            if (arr[factor] === 0) {
                const step = factor * 2 + 1;
                let start = factor * step + factor;

                const safe = sizeHalf - (step * 8);
                while (start < safe) {
                    arr[start] = 1; start += step;
                    arr[start] = 1; start += step;
                    arr[start] = 1; start += step;
                    arr[start] = 1; start += step;
                    arr[start] = 1; start += step;
                    arr[start] = 1; start += step;
                    arr[start] = 1; start += step;
                    arr[start] = 1; start += step;
                }
                while (start < sizeHalf) {
                    arr[start] = 1; start += step;
                }
            }
        }
        return this;
    }

    countPrimes() {
        let count = 1; // 2
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
        new PrimeSniperV5b(sieveSize).runSieve();
        nrOfPasses++;
    } while (performance.now() < timeFinish);

    callback(nrOfPasses);
}

const main = ({ sieveSize, timeLimitSeconds, verbose, runtime }) => {
    if (!new PrimeSniperV5b(sieveSize).runSieve().validatePrimeCount(verbose)) return;
    const timeStart = performance.now();
    runSieveBatch(sieveSize, timeLimitSeconds, (nrOfPasses) => {
        const dur = (performance.now() - timeStart) / NOW_UNITS_PER_SECOND;
        console.log(`helron-prime_sniper_v5b-${runtime};${nrOfPasses};${dur};1;algorithm=other,faithful=no,bits=8`);
    });
}

main(config);
