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
  private readonly sieveSize: number;
  private bits: Buffer;

  constructor(sieveSize: number) {
    this.sieveSize = sieveSize;
    this.bits = Buffer.alloc(sieveSize);
  }

  public runSieve() {
    let factor = 3;
    const q = Math.sqrt(this.sieveSize);

    while (factor <= q) {
      for (let num = factor; num < this.sieveSize; num += 2) {
        if (!this.bits[num]) {
          factor = num;
          break;
        }
      }

      for (let num = factor * factor; num < this.sieveSize; num += factor * 2) {
        this.bits[num] = 1;
      }

      factor += 2;
    }
  }

  public printResults(showResults: boolean, duration: number, passes: number) {
    if (showResults) process.stdout.write("2, ");

    let count = 1;
    for (let num = 3; num < this.sieveSize; num += 2) {
      if (!this.bits[num]) {
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

    console.log(`marghidanu;${passes};${duration};1;algorithm=base,faithful=yes`);
  }

  private countPrimes(): number {
    let count = 1;

    for (let num = 3; num < this.sieveSize; num += 2) {
      if (!this.bits[num]) count++;
    }

    return count;
  }

  private validateResults(): boolean {
    return DICT[this.sieveSize] === this.countPrimes();
  }
}

// --- Main "function"

let passes = 0;
const startTime = Date.now();

while (true) {
  const sieve = new PrimeSieve(1e6);
  sieve.runSieve();

  passes++;
  const duration = (Date.now() - startTime) / 1000;
  if (duration >= 5) {
    sieve.printResults(false, duration, passes);
    break;
  }
}
