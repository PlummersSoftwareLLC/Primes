/**
 * (Node)JS Prime Sieve
 * (Based on the PHP version)
 */

const primeCounts = {
    10: 4,
    100: 25,
    1000: 168,
    10000: 1229,
    100000: 9592,
    1000000: 78498,
    10000000: 664579,
    100000000: 5761455,
};

class PrimeSieve {

    constructor(sieveSize = 1000000) {
        this.sieveSize = sieveSize;
        let rawbitSize = (((this.sieveSize + 1) / 2)) | 0;
        this.rawbits = new Array(rawbitSize);
        this.rawbits.fill(1);
    }

    getBit(index) {
        if (index % 2 !== 0)
            return this.rawbits[(index / 2) | 0]
        return false;
    }

    clearBit(index) {
        if (index % 2 !== 0)
            this.rawbits[(index / 2) | 0] = false;
    }

    runSieve() {
        let factor = 3;
        let q = Math.sqrt(this.sieveSize);

        while (factor < q) {
            for (let i = factor; i <= this.sieveSize; i++) {
                if (this.getBit(i)) {
                    factor = i;
                    break;
                }
            }

            for (let i = factor * 3; i <= this.sieveSize; i += factor * 2) {
                this.clearBit(i);
            }

            factor += 2;
        }
    }

    printResult() {
        for (let i = 0; i < this.sieveSize; i++) {
            if (this.getBit(i))
                process.stdout.write(`${i}, `);
        }
    }

    getRawbitCount() {
        return this.rawbits.reduce((a, b) => a + b, 0);
    }

}

let tStart = Date.now();
let passes = 0;
let sieveSize = 1000000;
let printResult = false;
let rawbitCount = null;
let runTime = 10;

while ((Date.now() - tStart) < runTime * 1000) {
    let sieve = new PrimeSieve(sieveSize);
    sieve.runSieve();
    rawbitCount = sieve.getRawbitCount();
    passes++;

    if (printResult) {
        sieve.printResult();
        process.stdout.write('\n');
    }
}

let time = (Date.now() - tStart);
let valid = primeCounts[sieveSize] === rawbitCount ? "True" : "False";

console.log(`Passes: ${passes}, Time: ${time}, Avg: ${time/passes}, Limit: ${sieveSize}, Count: ${rawbitCount}, Valid: ${valid}`)