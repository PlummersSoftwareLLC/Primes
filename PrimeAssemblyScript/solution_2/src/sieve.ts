type BitWord = u32;

const BITS = <BitWord>(sizeof<BitWord>() * 8);

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
            if (unchecked(bits[factor / BITS]) & (1 << <BitWord>factor)) {
                continue;
            }
            start = (2 * factor) * (factor + 1);
            step = 2 * factor + 1;
            while (start < end) {
                unchecked(bits[start / BITS] |= 1 << <BitWord>start);
                start += step;
            }
        }
    }

    printResults(showResults: bool, duration: f32, passes: i32): void {
        const size = this.size;
        let count = 0;
        let len = (size + 1) / 2;
        for (let i: u32 = 0; i < len; i++) {
            if ((~this.bits[i / BITS] >> <BitWord>i) & 1) {
                if (showResults)
                    console.log(i.toString() + ", ");
                count++;
            }
        }

        console.log(
            "Passes: "   + passes.toString() +
            ", Time: "   + duration.toString() +
            ", Avg: "    + (duration / (passes as f32)).toString() +
            ", Limit: "  + this.size.toString() +
            ", Count1: " + count.toString() +
            ", Count2: " + this.countPrimes().toString() +
            ", Valid: "  + this.validateResults().toString()
        );

        // Following 2 lines added by rbergen to conform to drag race output format
        console.log("");
        console.log("maxgraey;" + passes.toString() + ";" + duration.toString() + ";1;algorithm=base,faithful=yes");

    }

    countPrimes(): u32 {
        const size = this.size;
        let count: u32 = 0;
        let len = (size + 1) / 2;
        for (let i: u32 = 0; i < len; i++) {
            count += (~this.bits[i / BITS] >> <BitWord>i) & 1;
        }
        return count;
    }
}
