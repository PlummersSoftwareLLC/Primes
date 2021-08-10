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
        const bits = this.bits;
        const end = (this.size + 1) / 2;
        const tail = (end + BITS - 1) / BITS;
        let factor: u32 = 0;

        while (true) {
            // search factor
            ++factor;
            while (
                factor < end &&
                (unchecked(bits[factor / BITS]) & (1 << factor as BitWord)) != 0
            ) ++factor;

            let start = (factor * 2) * (factor + 1);
            if (start >= end) break;

            let step = factor * 2 + 1;

            const s1 = step * 1;
            const s2 = step * 2;
            const s3 = step * 3;
            const s4 = step * 4;

            for (let i: u32 = 0; i < BITS; ++i) {
                let mask = 1 << start;
                let idx = start / BITS;
                start += step;

                while (idx + s4 < tail) {
                    unchecked(bits[idx +  0] |= mask);
                    unchecked(bits[idx + s1] |= mask);
                    unchecked(bits[idx + s2] |= mask);
                    unchecked(bits[idx + s3] |= mask);
                    idx += s4;
                }

                while (idx < tail) {
                    unchecked(bits[idx] |= mask);
                    idx += step;
                }
            }
        }
    }

    printResults(showResults: bool, duration: f32, passes: i32): void {
        const size = this.size;
        let count = 0;
        let len = (size + 1) / 2;
        for (let i: u32 = 0; i < len; ++i) {
            if (!(this.bits[i / BITS] & (1 << i as BitWord))) {
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
        console.log(`maxgraey_unrolled;${passes};${duration};1;algorithm=base,faithful=yes,bits=1`);
    }

    countPrimes(): u32 {
        const size = this.size;
        let count: u32 = 0;
        let len = (size + 1) / 2;
        for (let i: u32 = 0; i < len; ++i) {
            count += i32(!(this.bits[i / BITS] & (1 << i as BitWord)));
        }
        return count;
    }
}
