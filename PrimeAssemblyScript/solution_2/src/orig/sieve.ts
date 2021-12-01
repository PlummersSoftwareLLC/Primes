type BitWord = u32;

const BITS = (sizeof<BitWord>() * 8) as BitWord;

@final export class PrimeSieve {
    private static primeCounts: Map<u32, u32> = new Map<u32, u32>()
        .set(10, 4)
        .set(100, 25)
        .set(1000, 168)
        .set(10000, 1229)
        .set(100000, 9592)
        .set(1000000, 78498)
        .set(10000000, 664579)
        .set(100000000, 5761455);

    private bits: StaticArray<BitWord>;
    // @ts-ignore: decorator
    @inline constructor(private size: u32) {
        this.bits = new StaticArray((size + BITS * 2 - 1) / (BITS * 2));
    }

    validateResults(): bool {
        const primes = PrimeSieve.primeCounts;
        return (
            primes.has(this.size) &&
            primes.get(this.size) == this.countPrimes()
        );
    }

    @inline runSieve(): void {
        const size = this.size;
        const bits = this.bits;

        let start: u32, step: u32;
        let end = (size + 1) / 2;
        let q = Math.sqrt(size / 2) as u32;

        for (let factor: u32 = 1; factor <= q; ++factor) {
            if (unchecked(bits[factor / BITS]) & (1 << factor as BitWord)) {
                continue;
            }
            start = (2 * factor) * (factor + 1);
            step = 2 * factor + 1;
            while (start < end) {
                unchecked(bits[start / BITS] |= 1 << start as BitWord);
                start += step;
            }
        }
    }

    printResults(showResults: bool, duration: f32, passes: i32): void {
        const size = this.size;
        let count = 0;
        let len = (size + 1) / 2;
        for (let i: u32 = 0; i < len; i++) {
            if ((~this.bits[i / BITS] >> i as BitWord) & 1) {
                if (showResults)
                    console.log(i.toString() + ", ");
                count++;
            }
        }

        console.log(
            `Passes: ${passes}\
            , Time: ${duration}\
            , Avg: ${duration / (passes as f32)}\
            , Limit: ${this.size}\
            , Count1: ${count}\
            , Count2: ${this.countPrimes()}\
            , Valid: ${this.validateResults()}\n`
            .replaceAll("            ", "")
        );
        console.log(`maxgraey;${passes};${duration};1;algorithm=base,faithful=yes,bits=1`);
    }

    countPrimes(): u32 {
        const size = this.size;
        let count: u32 = 0;
        let len = (size + 1) / 2;
        for (let i: u32 = 0; i < len; i++) {
            count += (~this.bits[i / BITS] >> i as BitWord) & 1;
        }
        return count;
    }
}
