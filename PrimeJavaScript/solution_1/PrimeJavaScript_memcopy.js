/*
JavaScript implementation of Prime Sieve. This solution was formerly known
as PrimeNodeJS, however has been renamed because Node is the runtime, not
the language. Additional changes have been made to allow for running this
benchmark on other runtimes (currently Node, Bun, and Deno).

https://github.com/petkaantonov/bluebird/wiki/Optimization-killers
start with
node --trace-opt --trace-deopt --trace-ic PrimeNode_memcopy.js

Based on:
- contributions including using Deno by Dief Bell
- previous implementation of NodeJS/solution_1 by Frank van Bakel
- Python/solution_2 by ssovest
- MyFirstPython Program (tm) Dave Plummer 8/9/2018

*/

"use strict";
const NOW_UNITS_PER_SECOND =  1000;
const WORD_SIZE = 32;

let config = {
	sieveSize: 1000000,
	timeLimitSeconds: 5,
	verbose: false,
	runtime: '',
    workers: 1
};

try
{
	!!Deno;
	config.runtime = "deno";
	config.verbose = Deno.args.includes("verbose");
}
catch
{
	const { performance } = require('perf_hooks');
	const runtimeParts = process.argv[0].split("/");
	config.runtime = runtimeParts[runtimeParts.length - 1];
	config.verbose = process.argv.includes("verbose");
}
// 32-bit bitArray for javascript, with only needed functions
// int32, not uint and not 64bit because: javascript uses 32-bit int with bitwise operations
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Bitwise_AND
// bigint wont work because irt uses signed
// shifting not with >> but with >>> is for zero fill right shift
class bitArray {
	constructor(size) {
		this.wordArray = new Int32Array(1 + (size >>> 5));
	}

	setBitTrue(index) {
		const wordOffset = index >>> 5;  // 1 word = 2ˆ5 = 32 bit, so shift 5, much faster than /32
		const bitOffset = index & 31;  // use & (and) for remainder, faster than modulus of /32
		this.wordArray[wordOffset] |= (1 << bitOffset);
	}

	testBitTrue(index) {
		const wordOffset = index >>> 5;
		const bitOffset = index & 31;
		return this.wordArray[wordOffset] & (1 << bitOffset);  // returning result not as bool for performance
	}

	setBitsTrue(range_start, step, range_stop) {
		if (step > WORD_SIZE/2) { 
			// steps are large: check if the range is large enough to reuse the same mask
			let range_stop_unique =  range_start + 32 * step;
			if (range_stop_unique > range_stop) {
				// range is not large enough for repetition (32 * step)
				for (let index = range_start; index < range_stop; index += step) {
					this.setBitTrue(index);
				}
				return;
			}
			// range is large enough to reuse the mask
			const range_stop_word = range_stop >>> 5;
			for (let index = range_start; index < range_stop_unique; index += step) {
				let wordOffset = index >>> 5;
				const bitOffset = index & 31;
				const mask = (1 << bitOffset);
				do {
					this.wordArray[wordOffset] |= mask;
					wordOffset += step; // pattern repeats on word level after {step} words
				} while (wordOffset <= range_stop_word);
			}
			return;
		}

		// optimized for small sizes: set wordvalue multiple times before committing to memory
		let index = range_start;
		let wordOffset = index >>> 5;  // 1 word = 2ˆ5 = 32 bit, so shift 5, much faster than /32
		let wordValue = this.wordArray[wordOffset];

		while (index < range_stop) {
			const bitOffset = index & 31;  // use & (and) for remainder, faster than modulus of /32
			wordValue |= (1 << bitOffset);

			index += step;
			const newwordOffset = index >>> 5;  // 1 word = 2ˆ5 = 32 bit, so shift 5, much faster than /32
			if (newwordOffset != wordOffset) { // moving to new word: store value and get new value
				this.wordArray[wordOffset] = wordValue;
				wordOffset = newwordOffset;
				wordValue = this.wordArray[wordOffset];
			}
		}
		this.wordArray[wordOffset] = wordValue; // make sure last value is stored
	}

	searchBitFalse(index) {
	    while (this.testBitTrue(index)) { index++ };  // will stop automatically because bits were 0 filled
		return index;
	}

	// default size in bit
	// assumptions:
	// everything between source_start and destination start should be copied to destination_start and repeated until destination_shop

	copyPattern(source_start, destination_start, destination_stop)	{
		const size = destination_start - source_start;
		let copy_start = destination_start;

		if (size < WORD_SIZE*2) { // handle small: fill the second word
			let copy_max = WORD_SIZE*2 + source_start;
			for (let index=0; index<size; index++) {
				if (this.testBitTrue(source_start+index)) {
					let copy_index = destination_start+index;
					while (copy_index < copy_max) {
						this.setBitTrue(copy_index);
						copy_index += size;
					}
				}
			}
			while (copy_start < WORD_SIZE*2) copy_start += size;
		}

		let source_word = source_start >>> 5;
		let copy_word = copy_start >>> 5;
		let destination_stop_word = destination_stop >>> 5;
		let shift = (source_start & 31) - (copy_start & 31);
		let dest_wordValue = 0;

		if (shift > 0) {
            let shift_flipped = WORD_SIZE-shift;
            dest_wordValue = this.wordArray[source_word] >>> shift;
            dest_wordValue |= this.wordArray[source_word+1] << shift_flipped;
            this.wordArray[copy_word] |= dest_wordValue; // or the start in to not lose data
    
            while (copy_word++ <= destination_stop_word) {
                source_word++;
                dest_wordValue = this.wordArray[source_word] >>> shift;
                dest_wordValue |= this.wordArray[source_word+1] << shift_flipped;
                this.wordArray[copy_word] = dest_wordValue; 
			}
        }
		if (shift < 0) {
            shift = -shift;
            let shift_flipped = WORD_SIZE-shift;
            dest_wordValue = this.wordArray[source_word] << shift;
            dest_wordValue |= this.wordArray[source_word-1] >>> shift_flipped;
            this.wordArray[copy_word] |= dest_wordValue; // or the start in to not lose data
            while (copy_word++ <= destination_stop_word) {
                source_word++;
                dest_wordValue = this.wordArray[source_word] << shift;
                dest_wordValue |= this.wordArray[source_word-1] >>> shift_flipped;
                this.wordArray[copy_word] = dest_wordValue; 
            }
        }

        if (shift == 0) {
            while (copy_word++ <= destination_stop_word) {
                this.wordArray[copy_word]=this.wordArray[source_word];
                source_word++;
            }
        }
	}
}

/*
Main class for the prime calulation.
The bitArray stores only odd numbers, with formula number(index) = 2*index+1, e.g.
index = 0 -> number = 1
index = 1 -> number = 3
*/
class PrimeSieve {
	constructor(sieveSize) {
		this.sieveSize = sieveSize;
		this.sieveSizeInBits = sieveSize >>> 1;
		this.bitArray = new bitArray(1 + this.sieveSizeInBits);
	}

	runSieve() {
		let factor = 1;
		let blocksize_bits = 1; // a block is a repeating pattern of prime multiples, e.g. 3*5*7*32
		let range = 3;  // range is the maximum to project the product of the prime
		const q = Math.ceil(Math.sqrt(this.sieveSizeInBits));

		while (factor <= q) {
			const step = factor * 2 + 1;
			const start = factor * step + factor;

			if (range < this.sieveSizeInBits) { // check if we should copy previous results
				range = blocksize_bits * step * 2;  // range is x2 so the second block cointains all multiples of primes
				if (range > this.sieveSizeInBits) range = this.sieveSizeInBits;
				this.bitArray.copyPattern(blocksize_bits, blocksize_bits*2, range);
				blocksize_bits = blocksize_bits * step;
			}

			this.bitArray.setBitsTrue(start, step, range);
			factor = this.bitArray.searchBitFalse(factor + 1);
		}
		return this;
	}

	countPrimes() {
		let primeCount = 1;  // account for prime 2

		for (let index = 1; index < this.sieveSizeInBits; index++) {
			if (!this.bitArray.testBitTrue(index)) { // if bit is false, it's a prime, because non-primes are marked true
				primeCount++;
			}
		}

		return primeCount;
	}

	getPrimes(maxNr = 100) {
		const primeArray = [2];  // 2 is a special prime

		if (this.sieveSize > 1) {
			for (let factor = 1, count = 1; factor < this.sieveSizeInBits; factor++) {
				if (count >= maxNr) break;
				if (!this.bitArray.testBitTrue(factor)) {
					count = primeArray.push(factor * 2 + 1);
				}
			}
		}
		return primeArray;
	}

	validatePrimeCount(verbose) {
		// Historical data for validating our results - the number of primes
		// to be found under some limit, such as 168 primes under 1000
		const maxShowPrimes = 100;
		const knownPrimeCounts = {
			10: 4,
			100: 25,
			1000: 168,
			10000: 1229,
			100000: 9592,
			1000000: 78498,
			10000000: 664579,
			100000000: 5761455
		};
		const countedPrimes = this.countPrimes();
		const primeArray = this.getPrimes(maxShowPrimes);

		let validResult = false;
		if (this.sieveSize in knownPrimeCounts) {
			const knownPrimeCount = knownPrimeCounts[this.sieveSize];
			validResult = (knownPrimeCount == countedPrimes);
			if (!validResult)
				console.log(
					"\nError: invalid result.",
					`Limit for ${this.sieveSize} should be ${knownPrimeCount} `,
					`but result contains ${countedPrimes} primes`
				);
		}
		else console.log(
			`Warning: cannot validate result of ${countedPrimes} primes:`,
			`limit ${this.sieveSize} is not in the known list of number of primes!`
		);

		if (verbose)
			console.log(`\nThe first ${maxShowPrimes} found primes are:`, primeArray);
	
		return validResult;
	}
}

// run the sieve for timeLimitSeconds
function runSieveBatch(sieveSize, timeLimitSeconds=5, callback) {
    let nrOfPasses = 0;                                                 // Counter for the number of passes in a from timestart to timefinish

    const timeStart = performance.now();                                // Record starting time
    const timeFinish = timeStart + timeLimitSeconds * 1000;             // Calculate finish time before, so we don't repeat

    let sieve;                                                          // outside do loop to reference the last instance in verbose output
    do {
        sieve = new PrimeSieve(sieveSize);
        sieve.runSieve();
        nrOfPasses++;
    } while (performance.now() < timeFinish);                           // keep going for timeLimitSeconds

    callback(nrOfPasses);
}

// main procedure
const main = ({ sieveSize, timeLimitSeconds, verbose, runtime }) => {
	// validate algorithm - run one time
	const validResult = new PrimeSieve(sieveSize).runSieve().validatePrimeCount(verbose);
	if (!validResult) return false;

	//measure time running the batch
	const timeStart = performance.now();
	runSieveBatch(sieveSize, timeLimitSeconds, (nrOfPasses) => { // show off typical nodejs style
		const timeEnd = performance.now();
		const durationInSec = (timeEnd - timeStart) / NOW_UNITS_PER_SECOND;
		const totalPasses = nrOfPasses;
		console.log(`\nrogiervandam-memcopy-${runtime};${totalPasses};${durationInSec};1;algorithm=other,faithful=yes,bits=1`); 
	});
}

main(config);
