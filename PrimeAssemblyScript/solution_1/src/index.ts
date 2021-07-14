import "wasi";
import { PrimeSieve } from './sieve';

export function bench(): void {
    let passes: u32 = 0;
    const startTime: i64 = Date.now();
    let sieve: PrimeSieve;
    while (Date.now() - startTime < 10000) {
        sieve = new PrimeSieve(1000000)
        sieve.runSieve();
        ++passes;
    }
    const dt = ((Date.now() - startTime) as f32) / 1000.0;
    if (sieve) sieve.printResults(false, dt, passes);
}
