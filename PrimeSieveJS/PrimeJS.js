class PrimeSieve{
	constructor(size) {
		this.sieveSize = size;
		// JS supports fixed-size arrays, which has a significant performance improvement compared to its standard arrays.
		// There is no simple way to initialize it with a certain value for all elements, so storing the inverted values is faster.
		this.bitArray = new Uint8Array(size>>1);
		
		this.primeCounts = {
			10: 1,
			100: 25,
			1000: 168,
			10000 : 1229,
			100000 : 9592,
			1000000 : 78498,
			10000000 : 664579,
			100000000 : 5761455
		};
	}
	
	countPrimes() {
		let count = 0;
		for (let i = 0; i < this.bitArray.length; ++i) {
			if (!this.bitArray[i]) {
				++count;
			}
		}
		return count;
	}
	
	validateResults() {
		if (this.primeCounts[this.sieveSize]) 
			return this.primeCounts[this.sieveSize] == this.countPrimes();
		return false;
	}
	
	GetBit(index) {
		if (index % 2 == 0)
			return false;
		// JavaScript supports bitwise operations, which more than doubles its speed compared to parseInt(index/2)
		return !this.bitArray[index>>1];
		//return !this.bitArray[parseInt(index/2)];
	}
	
	ClearBit(index) {
		if (index % 2 == 0) {
			console.log('You are setting even bits, which is sub-optimal');
			return;
		}
		this.bitArray[index>>1] = true;
		//this.bitArray[parseInt(index/2)] = true;
	}
	
	runSieve() {
		let factor = 3;
		let q = parseInt(Math.sqrt(this.sieveSize));
		
		while (factor <= q) {
			for (let num = factor; num < this.sieveSize; ++num) {
				if (this.GetBit(num)) {
					factor = num;
					break;
				}
			}
			
			// If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
			// We can then step by factor * 2 because every second one is going to be even by definition
			
			for (let num = factor * 3; num <= this.sieveSize; num += factor<<1) {
				this.ClearBit(num);
			}
			
			factor += 2;
		}
	}
	
	printResults(showResults, duration, passes) {
		
		if (showResults) process.stdout.write('2, ');
		
		let count = 1;
		for (let num = 3; num <= this.sieveSize; ++num) {
			if (this.GetBit(num)) {
				if (showResults) {
					process.stdout.write(num + ', ');
				}
				++count;
			}
		}
		if (showResults) console.log('');
		console.log(`Passes: ${passes}, Time: ${duration}, Avg: ${duration / passes}, Limit: ${this.sieveSize}, Count: ${count}, Valid: ${this.validateResults()}`);
	}
}

var startTime = Date.now();
var passes = 0;
var sieve;

while (Date.now() - startTime < 10000) {
	sieve = new PrimeSieve(1000000)
	sieve.runSieve();
	++passes;
}

var dt = Date.now() - startTime;
if (sieve) sieve.printResults(false, dt.valueOf()/1000, passes);