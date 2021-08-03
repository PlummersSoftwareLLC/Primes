import { Bit8Array } from "./Bit8Array";
import { IBitArray } from "./IBitArray";
import { BitNumArray } from "./BitNumArray";
import { BitByteArray } from "./BitByteArray";
import { Bit32Array } from "./Bit32Array";

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

  public printResults(name: string, bitsPerFlag: number | undefined, duration: number, passes: number) {
    const avg = duration / passes;
    const countPrimes = this.countPrimes();
    const valid = this.validateResults();
    console.error(
      `Passes: ${passes}, Time: ${duration}, Avg: ${avg}, Limit: ${this.sieveSize}, Count: ${countPrimes}, Valid: ${valid}`
    );

    // Based on: marghidanu
    let tagLine = `mikevdbokke_${name};${passes};${duration};1;algorithm=base,faithful=yes`;
    if (bitsPerFlag) {
      tagLine += `,bits=${bitsPerFlag}`;
    }
    console.log(tagLine);
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
      sieve.printResults(bitArray.name, bitArray.bitsPerFlag, duration, passes);
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
  runOne((sieveSize: number) => {return new Bit32Array(sieveSize)}, sieveSize);
  doGC();
  
  runOne((sieveSize: number) => {return new Bit8Array(sieveSize)}, sieveSize);
  doGC();

  runOne((sieveSize: number) => {return new BitNumArray(sieveSize)}, sieveSize);
  doGC();
  
  runOne((sieveSize: number) => {return new BitByteArray(sieveSize)}, sieveSize);
  doGC();
}

main();
