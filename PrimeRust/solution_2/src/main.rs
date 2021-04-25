mod prime_object;
use prime_object::{Duration, Instant, PrimeSieve};

fn main() {
    let mut passes = 0;
    let time_start = Instant::now();
    let processing_time = 5; // Seconds

    loop {
        let mut prime = PrimeSieve::new(1_000_000);
        prime.run_sieve();

        passes += 1;

        if (Instant::now() - time_start) >= Duration::new(processing_time, 0) {
            let td = Instant::now() - time_start;
            prime.print_results(false, td, passes);
            break;
        }
    }
}
