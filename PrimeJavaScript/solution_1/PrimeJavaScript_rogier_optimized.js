"use strict";
const { performance } = require('perf_hooks');

const NOW_UNITS_PER_SECOND = 1000;
const WORD_SIZE = 32;

let config = { sieveSize: 1000000, timeLimitSeconds: 5, runtime: 'node' };

const sieveBits = config.sieveSize >>> 1;
const wordArraySize = 1 + (sieveBits >>> 5);
const GLOBAL_POOL = new Int32Array(wordArraySize);

function runSieveInline() {
    const arr = GLOBAL_POOL;
    arr.fill(0);

    let factor = 1;
    let blocksize_bits = 1;
    let range = 3;
    const q = Math.ceil(Math.sqrt(sieveBits));

    while (factor <= q) {
        const step = factor * 2 + 1;
        const start = factor * step + factor;

        if (range < sieveBits) {
            let dest_stop = blocksize_bits * step * 2;
            if (dest_stop > sieveBits) dest_stop = sieveBits;

            const source_start = blocksize_bits;
            const dest_start = blocksize_bits * 2;
            const size = dest_start - source_start;
            let copy_start = dest_start;

            if (size < 64) {
                let copy_max = 64 + source_start;
                for (let index = 0; index < size; index++) {
                    if (arr[(source_start + index) >>> 5] & (1 << ((source_start + index) & 31))) {
                        let copy_index = dest_start + index;
                        while (copy_index < copy_max) {
                            arr[copy_index >>> 5] |= (1 << (copy_index & 31));
                            copy_index += size;
                        }
                    }
                }
                while (copy_start < 64) copy_start += size;
            }

            let source_word = source_start >>> 5;
            let copy_word = copy_start >>> 5;
            let dest_stop_word = dest_stop >>> 5;
            let shift = (source_start & 31) - (copy_start & 31);
            let dest_wordValue = 0;

            if (shift > 0) {
                let shift_flipped = 32 - shift;
                dest_wordValue = arr[source_word] >>> shift;
                dest_wordValue |= arr[source_word + 1] << shift_flipped;
                arr[copy_word] |= dest_wordValue;

                while (copy_word++ <= dest_stop_word) {
                    source_word++;
                    dest_wordValue = arr[source_word] >>> shift;
                    dest_wordValue |= arr[source_word + 1] << shift_flipped;
                    arr[copy_word] = dest_wordValue;
                }
            }
            else if (shift < 0) {
                shift = -shift;
                let shift_flipped = 32 - shift;
                dest_wordValue = arr[source_word] << shift;
                dest_wordValue |= arr[source_word - 1] >>> shift_flipped;
                arr[copy_word] |= dest_wordValue;
                while (copy_word++ <= dest_stop_word) {
                    source_word++;
                    dest_wordValue = arr[source_word] << shift;
                    dest_wordValue |= arr[source_word - 1] >>> shift_flipped;
                    arr[copy_word] = dest_wordValue;
                }
            }
            else {
                while (copy_word++ <= dest_stop_word) {
                    arr[copy_word] = arr[source_word];
                    source_word++;
                }
            }

            blocksize_bits = blocksize_bits * step;
            range = blocksize_bits * step * 2;
            if (range > sieveBits) range = sieveBits;
        }

        // --- MARK MULTIPLES BLOCK (Inlined) ---
        if (step > 16) {
            let dest_stop_unique = start + 32 * step;
            if (dest_stop_unique > range) {
                for (let index = start; index < range; index += step) {
                    arr[index >>> 5] |= (1 << (index & 31));
                }
            } else {
                const range_stop_word = range >>> 5;
                for (let index = start; index < dest_stop_unique; index += step) {
                    let wordOffset = index >>> 5;
                    const mask = (1 << (index & 31));
                    do {
                        arr[wordOffset] |= mask;
                        wordOffset += step;
                    } while (wordOffset <= range_stop_word);
                }
            }
        } else {
            let index = start;
            let wordOffset = index >>> 5;
            let wordValue = arr[wordOffset];

            while (index < range) {
                wordValue |= (1 << (index & 31));
                index += step;
                const newwordOffset = index >>> 5;
                if (newwordOffset !== wordOffset) {
                    arr[wordOffset] = wordValue;
                    wordOffset = newwordOffset;
                    wordValue = arr[wordOffset];
                }
            }
            arr[wordOffset] = wordValue;
        }

        // Search Next
        let nextFactor = factor + 1;
        while (arr[nextFactor >>> 5] & (1 << (nextFactor & 31))) {
            nextFactor++;
        }
        factor = nextFactor;
    }
}

function countPrimes() {
    let primeCount = 1;
    const arr = GLOBAL_POOL;
    for (let index = 1; index < sieveBits; index++) {
        if ((arr[index >>> 5] & (1 << (index & 31))) === 0) primeCount++;
    }
    return primeCount;
}

function runSieveBatch(sieveSize, timeLimitSeconds = 5, callback) {
    let nrOfPasses = 0;
    const timeStart = performance.now();
    const timeFinish = timeStart + timeLimitSeconds * 1000;

    do {
        runSieveInline();
        nrOfPasses++;
    } while (performance.now() < timeFinish);

    callback(nrOfPasses);
}

const main = ({ sieveSize, timeLimitSeconds, runtime }) => {
    runSieveInline();
    const c = countPrimes();
    if (c !== 78498) {
        console.log("Validation Failed : " + c);
        return;
    }

    const timeStart = performance.now();
    runSieveBatch(sieveSize, timeLimitSeconds, (nrOfPasses) => {
        const dur = (performance.now() - timeStart) / NOW_UNITS_PER_SECOND;
        console.log(`\nhelron-rogier_optimized-${runtime};${nrOfPasses};${dur};1;algorithm=other,faithful=yes,bits=1`);
    });
}

main(config);
