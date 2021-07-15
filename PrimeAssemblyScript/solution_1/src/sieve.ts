export class PrimeSieve {
    private sieveSize: i32 = 0;
    private Bits: StaticArray<bool>;
    private static primeCounts: Map<i32, i32> = new Map<i32, i32>()
        .set(10, 4)
        .set(100, 25)
        .set(1000, 168)
        .set(10000, 1229)
        .set(100000, 9592)
        .set(1000000, 78498)
        .set(10000000, 664579)
        .set(100000000, 5761455);

    // @ts-ignore: decorator
    @inline constructor(size: u32) {
        this.sieveSize = size;
        // StaticArray did look a bit faster than UInt8Array in testing
        this.Bits = new StaticArray(size);
    }

    validateResults(): bool {
        if (PrimeSieve.primeCounts.has(this.sieveSize))
            return PrimeSieve.primeCounts.get(this.sieveSize) == this.countPrimes();
        return false;
    }

    @inline runSieve(): void {
        const size = this.sieveSize;
        const bits = this.Bits;
        let factor = 3;
        let q = Math.sqrt(size) as i32;

        while (factor <= q) {
            for (let num = factor; num < size; num += 2) {
                if (!unchecked(bits[num])) {
                    factor = num;
                    break;
                }
            }
            for (let num = factor * factor; num < size; num += factor * 2)
                unchecked(bits[num] = true);

            factor += 2;
        }
    }

    printResults(showResults: bool, duration: f32, passes: i32): void {
        const size = this.sieveSize;
        let count = i32(size >= 2);
        for (let num = 3; num <= size; num += 2) {
            if (!this.Bits[num]) {
                if (showResults)
                    console.log(num.toString() + ", ");
                count++;
            }
        }

        console.log("Passes: " + passes.toString() +
            ", Time: " + duration.toString() +
            ", Avg: " + (duration / (passes as f32)).toString() +
            ", Limit: " + this.sieveSize.toString() +
            ", Count1: " + count.toString() +
            ", Count2: " + this.countPrimes().toString() +
            ", Valid: " + this.validateResults().toString());

        // Following 2 lines added by rbergen to conform to drag race output format
        console.log("");
        console.log("donmahallem;" + passes.toString() + ";" + duration.toString() + ";1;algorithm=base,faithful=yes");

    }

    countPrimes(): i32 {
        const size = this.sieveSize;
        let count = i32(size >= 2);
        for (let i = 3; i < size; i += 2)
            if (this.Bits[i] == 0)
                count++;
        return count;
    }
}
