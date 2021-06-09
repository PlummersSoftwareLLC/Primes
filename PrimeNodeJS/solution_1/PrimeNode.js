/*
NodeJS implementation of Prime Sieve
Based on:
Python/solution_2 by ssovest
MyFirstPython Program (tm) Dave Plummer 8/9/2018

Author:    Frank van Bakel
Date:      2021-05-25
*/
'use strict';

/*
Global parameters, set by command line parameters
*/
var showResult      = false;
var isHelp          = false;
var isVerbose       = false;
var timeLimit       = 5;
var limit           = 1000000;
var maxNrOfPasses   = -1;

// if the showResult parameter is used, t
// then this defines the maximum number of
// prime numbers that will be shown
var maxShow         = 100;

// Historical data for validating our results - the number of primes
// to be found under some limit, such as 168 primes under 1000
var knownPrimeCounts = { 
    10 : 4, 
    100 : 25,                
    1000 : 168,
    10000 : 1229,
    100000 : 9592,
    1000000 : 78498,
    10000000 : 664579,
    100000000 : 5761455
  }

/*
Since javascript does not have a build in bit array,
we make our own simplified BitArray class
*/
class BitArray {
    constructor(size) {
        this.lenght = size;
        this.buffer = new ArrayBuffer(Math.ceil(this.lenght /32)*4)
        this.wordArray = new Uint32Array(this.buffer);
    }

    get size() {
        return this.lenght;
    }

    /*
    Sets a bit at the given index to the given value
    */
    setBit(index,value) {
        var wordOffset = Math.floor(index / 32);
        var bitOffset = index - wordOffset * 32;
        if (value) {
            this.wordArray[wordOffset] |= (1 << bitOffset);
        } else {
            this.wordArray[wordOffset] &= ~(1 << bitOffset);
        }
    }

    /*
    Get the bit value at the given index
    */
    getBit(index) {
        var wordOffset = Math.floor(index / 32);
        var bitOffset = index - wordOffset * 32;
        return !! (this.wordArray[wordOffset] & (1 << bitOffset));
    }

    /*
    Given an index, search in the BitArray for the next index with the
    requested value
    */
    searchNextBit(index,value) {
        var nextIndex = index +1;
        while (nextIndex < this.lenght) {
            if (this.getBit(nextIndex) == value) {
                return nextIndex;
            }
            nextIndex++;
        }
        // If we get here, there was no next value, 
        return NaN;
    }

    /*
    Return the string representation
    */
    toString() {
        return this.toArray().map(function(value) {
            return value ? '1': '0';
        }).join('');
    };

    /**
     * Returns the total number of bits set to one in this BitArray.
     */
    count(value) {
        var total = 0;
        var nextIndex = 0;
        while (nextIndex < this.lenght) {
            if (this.getBit(nextIndex) == value) {
                total++;
            }
            nextIndex++;
        }
        return total;
    }

    /**
    * Inverts this BitArray.
    */
    invert() {
        for (var i = 0; i < this.wordArray.length; i++) {
            this.wordArray[i] = ~(this.wordArray[i]);
        }
        return this;
    }
    
}
/*
Main class for the prime calulation
*/
class PrimeSieve {
    constructor() {
        this.size = limit;
        this.bits = new BitArray(Math.floor(this.size) / 2);
        // set all bits to 1
        this.bits.invert();
    }

    /*
    Look up our count of primes in the historical data (if we have it)
    to see if it matches
    */
    validateResults() {
        if (this.size in knownPrimeCounts) {
            if (knownPrimeCounts[this.size] == this.countPrimes()) {
                return true;
            }
        } else {
            console.log("Error: limit %i is not in the known list of number of primes!",this.size)
        }
        return false;
    }
        
    /*
    Calculate the primes up to the specified limit
    */
    runSieve() {
        var factor = 1;
        var q = Math.sqrt(this.size);

        while ( factor <= q ) {
            // If marking factor 3, you wouldn't mark 6 (it's a mult of 2) so start with the 3rd instance of this factor's multiple.
            // We can then step by factor * 2 because every second one is going to be even by definition
            var start = factor * 3 + 1;
            var step  = factor * 2 + 1;
            var i;
            for (i=start;i<this.bits.size;i=i+step) {
                this.bits.setBit(i,false);
            }

            factor = this.bits.searchNextBit(factor,true);
            if (isNaN(factor)) {
                break;
            }

        }
    }

    /*
    Return the count of bits that are still set in the sieve.
    Assumes you've already called runSieve, of course!
    */
    countPrimes() {
        return this.bits.count(1); // The 0 index is always 1, this counts 1
    }

    /*
    Returns the first found prime numbers, 
    limited to the maximum number of primenumbers defined by maxNr
    all if maxNr = -1
    Requires a prior runSieve call
    */
    getPrimes(maxNr = 20) {
        var primes = [];
        var nrFound = 0;
        if (this.size > 1) {
            nrFound = primes.push(2) // Since we auto-filter evens, we have to special case the number 2 which is prime
        }  
        if (this.size > 2) {
            // start at index 1, this represents prime number 3
            var num = 1;
            while (!isNaN(num)) {
                nrFound = primes.push( num * 2 + 1);
                num = this.bits.searchNextBit(num,true);
                if (nrFound == maxNr ) {
                    break;
                }
            }
        }
        return primes;
    }

    /*
    Displays the primes found, options
    - only the count of primes + the statistics
    - only the first found to the max number
    */
    printResults(showResult, duration, passes) {
        var count = this.countPrimes();

        if (showResult) {
            var i =0;
            var primes = this.getPrimes(maxShow); 
            console.log("The first %i found primes are: ", maxShow, primes);
          /*  for (var primeNr in this.getPrimes(maxShow)) {
                i++;
                console.log("Nr: %i, %i",i,primeNr);
            }
            */
            console.log();
        }
        if (isVerbose) {
            console.log("Passes: %i, Time: %f, Avg: %f (sec/pass), Limit: %i, Count: %i, Valid: %s",
                        passes, 
                        parseFloat(duration).toFixed(2), 
                        parseFloat(duration/passes).toFixed(8), 
                        this.size, 
                        count, 
                        new Boolean(this.validateResults()).toString()
                    );
        }

        // Following 2 lines are to conform to drag race output format
        console.log();
        console.log("fvbakelnodejs;%i;%f;1;algorithm=base,faithful=yes,bits=1", passes, duration);
    }


}

/*
Helper function to parse the command line arguments and fill paramers

Using this simplified method to avoid any dependency on modules
*/
function parseArguments() {
    
    var args = process.argv.slice(2);
    var i =0;

    while (i < args.length) {
        if (args[i] == '-h') {isHelp=true;}
        else if (args[i] == '-s') {showResult=true;}
        else if (args[i] == '-v') {isVerbose=true;}
        else if (args[i] == '-l') {
            if (i != args.length -1) {
                i++;
                limit = parseInt(args[i]);
                if (isNaN(limit)) {
                    console.error('Error -l is not a number');    
                }
            } else {
                console.error('Error: -l is missing parameter');
            }
        } 
        else if (args[i] == '-t') {
            if (i != args.length -1) {
                i++;
                timeLimit = parseInt(args[i]);
                if (isNaN(timeLimit)) {
                    console.error('Error: -t is not a number');    
                }
            } else {
                console.error('Error: -t is missing parameter');
            }
        } 
        else if (args[i] == '-m') {
            if (i != args.length -1) {
                i++;
                maxNrOfPasses = parseInt(args[i]);
                if (isNaN(maxNrOfPasses)) {
                    console.error('Error: -m is not a number');    
                }
            } else {
                console.error('Error: -m is missing parameter');
            }
        } 
        else {
            console.error('Error: Unsupported parameter: ' + args[i]);
            console.error('Correct usage is:');
            console.error('');
            printHelp();
        }
        i++;
    }
}

/*
Print the help information
*/
function printHelp() {
    console.log("Find prime numbers and measure performance");
    console.log("Usage:");
    console.log("node PrimeNode.js [-h] [-l <limit>] [-t <time limit>] [-s] [-m <max nr of passes>] [-v]");
    console.log("   -h              Show this help message");
    console.log("   -l <limit>      Upper search limit for calculating prime numbers, default=1000000");
    console.log("   -t <time limit> Minimal runtime in seconds, default=10");
    console.log("   -s              Write the found prime numbers, default disabled");
    console.log("   -m              Maximum number of passes, this overides the -t parameter, default disabled");
    console.log("   -v              Write extra verbose messages, default disabled");
}

function printConfig() {
    console.log("Effective configuration:");
        console.log("   Upper search limit (-l)           = %i",limit);
    if (maxNrOfPasses == -1) {
        console.log("   Minimal runtime (-t)              = %i seconds",timeLimit);
    } else {
        console.log("   Minimal runtime (-t)              = Not used");
    }
    if (maxNrOfPasses != -1) {
        console.log("   Maximum number of passes (-m)     = %i",maxNrOfPasses);
    } else {
        console.log("   Maximum number of passes (-m)     = Not used");
    }
    if (showResult) {
        console.log("   Write the first %i prime numbers = enabled",maxShow);
    } else {
        console.log("   Write the first %i prime numbers = disabled",maxShow);
    }
    if (isVerbose) {
        console.log("   Write extra verbose messages (-v) = enabled");
    } else {
        console.log("   Write extra verbose messages (-v) = disabled");
    }
}


/*
Main entry
*/
function main() {
    
    parseArguments();
    if (isHelp) {
        printHelp()
    } else {

        if (isVerbose) {
            console.log("Program starts at: %s",new Date());
            printConfig();            
        }
        var timeoutInMs = timeLimit * 1000;
        var sieve =new PrimeSieve(1);               // failsave incase the while loop has no results
        var timeStart = new Date();                 // Record our starting time
        var nrOfPasses = 0;                         // We're going to count how many passes we make in fixed window of time

        while (                                      
                   (new Date() - timeStart < timeoutInMs && maxNrOfPasses == -1) // Run until more than 10 seconds have elapsed
                || (nrOfPasses < maxNrOfPasses || maxNrOfPasses != -1)   // Altertnative stop after nr of passes
              ) {                                   
            sieve = new PrimeSieve(limit);          // Calc the primes up to the limit
            sieve.runSieve();                       // Find the results
            nrOfPasses++;                           // Count this pass
        }

        var timeDeltaInMs = new Date() - timeStart; // After the "at least 10 seconds", get the actual elapsed
        var durationInSec = timeDeltaInMs / 1000;
        sieve.printResults(showResult, durationInSec, nrOfPasses);

    }
    if (isVerbose) {console.log("Program ends at: %s", new Date());}    
}

main();