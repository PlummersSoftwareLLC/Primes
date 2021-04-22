export class PrimeSieve {
    sieveSize: i32 = 0;
    private Bits: StaticArray<bool>;
    private static primeCounts: Map<i32, i32> = new Map<i32, i32>().set(10, 4)
        .set(100, 25)
        .set(1000, 168)
        .set(10000, 1229)
        .set(100000, 9592)
        .set(1000000, 78498)
        .set(10000000, 664579)
        .set(100000000, 5761455);
    public constructor(size: u32) {
        this.sieveSize = size;
        // StaticArray did look a bit faster than UInt8Array in testing
        this.Bits = new StaticArray<bool>(size);

    }
    validateResults(): bool {
        if (PrimeSieve.primeCounts[this.sieveSize])
            return PrimeSieve.primeCounts[this.sieveSize] == this.countPrimes();
        return false;
    }

    runSieve(): void {
        const size: i32 = this.sieveSize;
        let factor: i32 = 3;
        let q: i32 = Math.sqrt(size) as i32;

        while (factor <= q) {
            for (let num: i32 = factor; num < size; num += 2) {
                if (!this.Bits[num]) {
                    factor = num;
                    break;
                }
            }
            for (let num: i32 = factor * factor; num < size; num += factor * 2)
                this.Bits[num] = true;

            factor += 2;
        }
    }

    printResults(showResults: bool, duration: f32, passes: i32): void {

        let count: i32 = (this.sieveSize >= 2);
        for (let num: i32 = 3; num <= this.sieveSize; num += 2) {
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
    }

    countPrimes(): i32 {
        let count: i32 = (this.sieveSize >= 2);
        for (let i: i32 = 3; i < this.sieveSize; i += 2)
            if (this.Bits[i] == 0)
                count++;
        return count;
    }
}
