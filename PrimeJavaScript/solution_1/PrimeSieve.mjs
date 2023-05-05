/*
Main class for the prime calulation. Moved to this file as it is
required by both PrimeJavaScript_worker_main.mjs and PrimeJavaScript_worker_child.mjs.

The BitArray stores only odd numbers, with formula number(index) = 2*index+1, e.g.
index = 0 -> number = 1
index = 1 -> number = 3
*/

// 32-bit bitarray for javascript, with only needed functions
// int32, not uint and not 64bit because: javascript uses 32int
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Bitwise_AND
// shifting not with >> but with >>> is for zero fill right shift
class BitArray
{
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
		return this.wordArray[wordOffset] & (1 << bitOffset); // use a mask to only get the bit at position bitOffset. >0=true, 0=false
	}
}

export class PrimeSieve
{
	constructor(sieveSize)
	{
		this.sieveSize = sieveSize;
		this.oddsize = sieveSize >>> 1;
		this.bitarray = new BitArray(1 + this.oddsize);
	}

	runSieve()
	{
		const q = Math.ceil(Math.sqrt(this.oddsize));  // convert to integer with ceil

		for (let factor = 1; factor <= q; factor++)
		{
			if (!this.bitarray.testBitTrue(factor))
			{
				const step = factor * 2 + 1;
				const start = factor * factor * 2 + factor + factor;

				for (let multiple = start; multiple < this.oddsize; multiple = multiple + step)
				{
					this.bitarray.setBitTrue(multiple);  // mark every multiple of this prime
				}
			}
		}
	}

	countPrimes()
	{
		let total = 1;  // account for prime 2

		for (let index = 1; index < this.oddsize; index++)
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
		const primes = [2]; // 2 is a special prime
		for (let factor = 1, count = 0; factor < this.oddsize; factor++)
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
