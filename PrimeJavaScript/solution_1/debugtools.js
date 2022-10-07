// tools for debugging
// copy these to the main file when debugging. 

function printword(word, description) {
	let row='';
	for(let i=0; i<32; i++) {
		row += (word & (1<<i))?'1':'.';
	}
	if (description) row += ' - ' + description;
	console.log('word:',row);
}

function deepAnalyzePrimes() {
    const sieve = this;

    // console.log("DeepAnalyzing");

    const range_to = sieve.sieveSizeInBits;

    for (let factor = 1, warn_prime = 0, warn_nonprime = 0; factor < range_to; factor++) {
        if (!this.bitArray.testBitTrue(factor)) { // is this a prime?
            const q = (Math.sqrt(factor * 2 + 1) | 1) * 2 + 1;

            for (let c = 1; c <= q; c++) {
                if ((factor * 2 + 1) % (c * 2 + 1) == 0 && (c * 2 + 1) != (factor * 2 + 1)) {
                    if (warn_prime++ < 30) {
                        console.log("Number %d (%d) was marked prime, but %d * %d = %d", factor * 2 + 1, factor, c * 2 + 1, (factor * 2 + 1) / (c * 2 + 1), factor * 2 + 1);
                    }
                }
            }
        }
        else {
            const q = (Math.sqrt(factor * 2 + 1) | 1) * 2 + 1;
            let c_factor = 0;

            for (let c = 1; c <= q; c++) {
                if ((factor * 2 + 1) % (c * 2 + 1) == 0 && (c * 2 + 1) != (factor * 2 + 1)) {
                    c_factor++;
                    break;
                }
            }

            if (c_factor == 0) {
                if (warn_nonprime++ < 30) {
                    console.log("Number %d (%d) was marked non-prime, but no factors found. So it is prime", factor * 2 + 1, factor);
                }
            }
        }
    }
}