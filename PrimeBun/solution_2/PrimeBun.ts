/**
 * Bun implementation of Prime Sieve.
 * See Bun project at https://bun.sh
 * 
 * This is essentially a copy of Rogier van Dam's
 * NodeJS implementation PrimeNodeJS/solution_1 (as of 61ab3fa9576fee399b8d4eac9af14e2e2d2c54fe)
 * except for:
 *      - removal of the use of perf-hooks
 *      - new time scale factor for timers
 *      - reformat for personal taste ;)

 * Author:    Dief Bell
 * Date:      2022-07-08
 */
'use strict';

// messy hack because I couldn't work out how to get it to stop complaining about "can't find variable: exports"
const PrimeBun = () =>
{

// performance.now() in Bun returns time since process started in nanoseconds
const NANOSECONDS_PER_SECOND = 1000000000;


// Historical data for validating our results - the number of primes
// to be found under some limit, such as 168 primes under 1000
const knownPrimeCounts = {
    10: 4,
    100: 25,
    1000: 168,
    10000: 1229,
    100000: 9592,
    1000000: 78498,
    10000000: 664579,
    100000000: 5761455
}



// 32-bit bitarray for javascript, with only needed functions
// int32, not uint and not 64bit because: javascript uses 32int
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Bitwise_AND
// shifting not with >> but with >>> is for zero fill right shift
class BitArray
{
    private wordArray : Int32Array;
    constructor(size)
    {
        this.wordArray = new Int32Array(1 + (size >>> 5));  // allocation in javascript is with 0
    }

    setBitTrue(index)
    {
        const wordOffset = index >>> 5;  // 1 word = 2ˆ5 = 32 bit, so shift 5, much faster than /32
        const bitOffset = index & 31;  // use & (and) for remainder, faster than modulus of /32
        this.wordArray[wordOffset] |= (1 << bitOffset);
    }

    testBitTrue(index)
    {
        const wordOffset = index >>> 5;
        const bitOffset = index & 31;
        return this.wordArray[wordOffset] & (1 << bitOffset);  // use a mask to only get the bit at position bitOffset. >0=true, 0=false
    }
}



/*
* Main class for the prime calulation.
* The BitArray stores only odd numbers, with formula number(index) = 2*index+1, e.g.
* index = 0 -> number = 1
* index = 1 -> number = 3
*/
class PrimeSieve
{
    private sieveSize : number;
    private oddSize : number;
    private bitarray : BitArray;

    constructor(sieveSize)
    {
        this.sieveSize = sieveSize;
        this.oddSize = sieveSize >>> 1;
        this.bitarray = new BitArray(1 + this.oddSize);
    }

    runSieve()
    {
        const q = Math.ceil(Math.sqrt(this.oddSize));  // convert to integer with ceil

        for (let factor = 1; factor <= q; factor++)
        {
            if (!this.bitarray.testBitTrue(factor))
            {
                const step = factor * 2 + 1;
                const start = factor * factor * 2 + factor + factor;

                for (let multiple = start; multiple < this.oddSize; multiple = multiple + step)
                {
                    this.bitarray.setBitTrue(multiple);  // mark every multiple of this prime
                }
            }
        }
    }

    countPrimes()
    {
        let total = 1;  // account for prime 2

        for (let index = 1; index < this.oddSize; index++)
        {
            if (!this.bitarray.testBitTrue(index))  // if bit is false, it's a prime, because non-primes are marked true
            {
                total++;
            }
        }

        return total;
    }

    getPrimes(max = 100)
    {
        const primes = [2];  // 2 is a special prime

        for (let factor = 1, count = 0; factor < this.oddSize; factor++)
        {
            if (count >= max) break;
            if (!this.bitarray.testBitTrue(factor))
            {
                count = primes.push(factor * 2 + 1);
            }
        }

        return primes;
    }

}



// run the sieve for timeLimitSeconds
const runSieveBatch = (sieveSize, timeLimitSeconds = 5) =>
{
    let nrOfPasses = 0;  // Counter for the number of passes in a from timestart to timefinish

    const timeStart = performance.now();  // Record starting time
    const timeFinish = timeStart + timeLimitSeconds * NANOSECONDS_PER_SECOND;  // Calculate finish time before, so we don't repeat

    do
    {
        const sieve = new PrimeSieve(sieveSize);
        sieve.runSieve();
        nrOfPasses++;
    }
    while (performance.now() < timeFinish);  // keep going for timeLimitSeconds

    return nrOfPasses;
}



// get a single sieve (for validation and statistics)
const evalSieve = (sieveSize, maxShowPrimes = 100) =>
{
    const sieve = new PrimeSieve(sieveSize);
    sieve.runSieve();
    return {
        "countedPrimes": sieve.countPrimes(),
        "primeArray": sieve.getPrimes(maxShowPrimes)
    }
}



interface IConfig
{
    sieveSize : number;
    timeLimitSeconds : number;
    verbose : boolean;
    maxShowPrimes : number;
}



/*
 * main procedure
*/
const main = (config : IConfig) =>
{
    // run once, without threads
    const { sieveSize, timeLimitSeconds, maxShowPrimes, verbose } = config;

    //measure time running the batch
    const timeStart = performance.now();
    const totalPasses = runSieveBatch(sieveSize, timeLimitSeconds);
    const timeEnd = performance.now();
    const durationInSec = (timeEnd - timeStart) / NANOSECONDS_PER_SECOND;

    // validate algorithm - run one final time on the result
    const sieveResult = evalSieve(sieveSize);
    let validResult = false;
    if (sieveSize in knownPrimeCounts)
    {
        const knownPrimeCount = knownPrimeCounts[sieveSize];
        validResult = (knownPrimeCount == sieveResult.countedPrimes);

        if (!validResult)
            console.log(
                `\nError: invalid result. \
                Limit for ${sieveSize} should be ${knownPrimeCount} but result contains ${sieveResult.countedPrimes} primes`
            );
    }
    else
    {
        console.log(
            `Warning: cannot validate result of ${sieveResult.countedPrimes} primes:`,
            `limit ${sieveSize} is not in the known list of number of primes!`
        );
    }

    if (validResult)
    {
        const label = process.argv[1].endsWith(".js")
            ? "\ndiefbell_tsc"
            : "\ndiefbell_ts-bun";
        const msg = [
            label,  // label
            totalPasses.toString(),
            durationInSec.toString(),
            "1",  // threads
            "algorithm=base,faithful=yes,bits=1"  // other info
        ];
        console.log(msg.join(";"))
    }

    if (verbose)
    {
        console.log(`\nThe first ${maxShowPrimes} found primes are:`, sieveResult.primeArray);

        console.log(
            `Passes: ${totalPasses},`,
            `Time: ${(durationInSec).toFixed(2)},`,
            `Avg: ${(durationInSec / totalPasses).toFixed(8)} (sec/pass),`,
            `Sieve size: ${sieveSize},`,
            `Primes: ${sieveResult.countedPrimes},`,
            `Valid: ${validResult}`
        );
    }
}



const defaultVerbose = false;
const verbose = (process.argv[2] && process.argv[2] === "verbose") || defaultVerbose;

const config : IConfig = {
    sieveSize: 1000000,
    timeLimitSeconds: 15,
    verbose: verbose,
    maxShowPrimes: verbose ? 100 : 0
}



main(config);

}
PrimeBun();