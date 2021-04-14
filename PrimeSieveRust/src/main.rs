mod prime_sieve;

use prime_sieve::PrimeSieve;
use stopwatch::Stopwatch;

fn main() {
    let started_at = Stopwatch::start_new();
    let mut passes = 0;
    let mut sieve = PrimeSieve::none();

    while started_at.elapsed().as_secs_f32() < 10_f32 {
        sieve = PrimeSieve::new(1_000_000);
        sieve.run_sieve();
        passes = passes + 1;
    }

    let running_time = started_at.elapsed();
    sieve.print_results(false, running_time.as_secs_f32(), passes);
}
