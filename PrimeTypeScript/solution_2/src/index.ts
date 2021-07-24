import { Bit8Array } from "./Bit8Array";
import { IBitArray } from "./IBitArray";
import { BitNumArray } from "./BitNumArray";
import { BitByteArray } from "./BitByteArray";
import { Bit32Array } from "./Bit32Array";
import { Solution1ByteBuffer } from './Solution1ByteBuffer'

const DICT: { [size: number]: number } = {
  10: 4,
  100: 25,
  1000: 168,
  10000: 1229,
  100000: 9592,
  1000000: 78498,
  10000000: 664579,
  100000000: 5761455,
  1000000000: 50847534,
  10000000000: 455052511,
};

class PrimeSieve {
  public readonly sieveSize: number;

  constructor(
    private readonly bits: IBitArray
  ) {
    this.sieveSize = bits.size;
    // Solution #1 - Passes: 3516 - 8 byte / bit
    // this.bits = new BitNumArray(sieveSize);  // Passes: 195 - number / bit
    // this.bits = new BitByteArray(sieveSize);  // Passes: 3248 - 8 byte / bit
    // this.bits = new Bit8Array(sieveSize);  // Passes: 3965 - 1 bit / bit

    // Fastest so far
    // this.bits = new Bit32Array(sieveSize); // Passes: 4286 - 1 bit / bit
  }

  public runSieve() {
    let factor = 3;
    const q = Math.sqrt(this.sieveSize);

    while (factor <= q) {
      for (let num = factor; num < this.sieveSize; num += 2) {
        if (!this.bits.get(num)) {
          factor = num;
          break;
        }
      }

      const inc = factor * 2; // increased speed

      for (let num = factor * factor; num < this.sieveSize; num += inc) {
        this.bits.setTrue(num);
      }

      factor += 2;
    }
  }

  public printResults(showResults: boolean, name: string, bitsPerFlag: number, duration: number, passes: number) {
    if (showResults) process.stdout.write("2, ");

    let count = 1;
    for (let num = 3; num < this.sieveSize; num += 2) {
      if (!this.bits.get(num)) {
        if (showResults) process.stdout.write(`${num}, `);
        count++;
      }
    }

    if (showResults) console.log();

    // TODO: Add old style too
    const avg = duration / passes;
    const countPrimes = this.countPrimes();
    const valid = this.validateResults();
    console.error(
      `Passes: ${passes}, Time: ${duration}, Avg: ${avg}, Limit: ${this.sieveSize}, Count1: ${countPrimes}, Count2: ${count}, Valid: ${valid}`
    );

    // Based on: marghidanu
    console.log(`mikevdbokke/${name};${passes};${duration};${bitsPerFlag};algorithm=base,faithful=yes`);
  }

  private countPrimes(): number {
    let count = 1;

    for (let num = 3; num < this.sieveSize; num += 2) {
      if (!this.bits.get(num)) count++;
    }

    return count;
  }

  private validateResults(): boolean {
    return DICT[this.sieveSize] === this.countPrimes();
  }
}

// --- Main "function"

let passes = 0;
let startTime = Date.now();
const sieveSize = 1e6;

function runOne(createArray: (sieveSize: number) => IBitArray, sieveSize: number) {
  passes = 0;
  startTime = Date.now();
  while (true) {
    const bitArray: IBitArray = createArray(sieveSize);
    const sieve = new PrimeSieve(bitArray);
    sieve.runSieve();
  
    passes++;
    const duration = (Date.now() - startTime) / 1000;
    if (duration >= 5) {
      sieve.printResults(false, bitArray.name, bitArray.bitsPerFlag, duration, passes);
      break;
    }
  }
}

function doGC() {
  // Force cleanup inbetween runs, so it does not effect other run.
  startTime = Date.now();
  global.gc();
  global.gc();
  global.gc();
  global.gc();
  const duration = (Date.now() - startTime) / 1000;
  
  //console.error(`GC Time: ${duration}`);
}

function main() {
  doGC();

  runOne((sieveSize: number) => {return new BitNumArray(sieveSize)}, sieveSize);

  doGC();

  runOne((sieveSize: number) => {return new Solution1ByteBuffer(sieveSize)}, sieveSize);

  doGC();
  
  runOne((sieveSize: number) => {return new BitByteArray(sieveSize)}, sieveSize);
  
  doGC();
  
  runOne((sieveSize: number) => {return new Solution1ByteBuffer(sieveSize)}, sieveSize);
  
  doGC();
  
  runOne((sieveSize: number) => {return new Bit8Array(sieveSize)}, sieveSize);
  
  doGC();
  
  runOne((sieveSize: number) => {return new Bit32Array(sieveSize)}, sieveSize);
  
  doGC();
}

let is32only = false;
for (let arg of process.argv) {
  if (arg=='--32only') {
    is32only = true;
  }
}
doGC();

if (is32only) {
  runOne((sieveSize: number) => {return new Bit32Array(sieveSize)}, sieveSize);
} else {
  main();
}
