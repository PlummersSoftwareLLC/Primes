mod prime_object;
use prime_object::{Duration, Instant, PrimeSieve};

fn main() {
    let mut passes = 0;
    let time_start = Instant::now();

    loop {
        let prime = PrimeSieve::new(1000);
        prime.run_sieve();

        passes += 1;

        if (Instant::now() - time_start) >= Duration::new(10, 0) {
            let td = Instant::now() - time_start;
            prime.print_results(false, td, 1);
            break;
        }
    }

}
