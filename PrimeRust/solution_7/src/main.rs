use std::time::{Duration, Instant};
mod wheel_sieve;
use crate::wheel_sieve::WheelSieve;

/// ## Print header
///
/// > Helper function that displays information about a run.
fn print_header(threads: usize, limit: usize, run_duration: Duration) {
    println!(
        "\nGenerating primes up to {}...\nNumber of threads: {}.\nRun duration (in seconds): {}\n",
        limit,
        threads,
        run_duration.as_secs()
    );
}

/// ## Print output
///
/// > Helper function that displays the results of a run.
fn print_output(total_passes: usize, run_duration: Duration, num_threads: usize) {
    println!("Results:\n");
    println!(
        "sergiocks;{};{};{};algorithm=wheel,faithful=yes,bits=8\n",
        total_passes,
        run_duration.as_secs_f64(),
        num_threads
    );
}

/// ## Run Implementation
///
/// > Helper function that spawns threads, runs the algorithm, collects the results and displays
///   the output.
fn run_implementation(num_threads: usize, limit: usize, run_duration: Duration) {
    let start_time = Instant::now();
    let threads: Vec<_> = (0..num_threads)
        .map(|_| {
            std::thread::spawn(move || {
                let mut local_passes: usize = 0;
                while (Instant::now() - start_time) < run_duration {
                    let mut sieve = WheelSieve::new(limit / 2);
                    sieve.run();
                    local_passes += 1;
                }
                local_passes
            })
        })
        .collect();
    let results: Vec<_> = threads.into_iter().map(|t| t.join().unwrap()).collect();
    let end_time = Instant::now();
    let total_passes: usize = results.iter().map(|r| r).sum();

    // Print the results.
    print_output(total_passes, end_time - start_time, num_threads);
}

/// ## Main function
///
/// > Entrypoint of the program.
///
/// Runs the implementation two times:
///
/// 1. On a single thread.
/// 2. On as many threads as the CPU supports.
fn main() {
    let threads = num_cpus::get();
    let limit = 1_000_000;
    let run_duration = Duration::new(5, 0);

    // 1. Single thread run.
    print_header(1, limit, run_duration);
    run_implementation(1, limit, run_duration);

    // 2. Multi-thread run.
    print_header(threads, limit, run_duration);
    run_implementation(threads, limit, run_duration);
}

/// ## Tests
///
/// > Tests the correctness of the implementation.
///
/// Test the number of primes found against known numbers for different ranges.
/// The tests are run when building the Docker image.
#[cfg(test)]
mod tests {
    use crate::wheel_sieve::WheelSieve;

    //#region ===== Tests for specific limits =====
    #[test]
    fn it_works_limit_10() {
        it_works(5);
    }
    #[test]
    fn it_works_limit_100() {
        it_works(50);
    }
    #[test]
    fn it_works_limit_1_000() {
        it_works(500);
    }
    #[test]
    fn it_works_limit_10_000() {
        it_works(5_000);
    }
    #[test]
    fn it_works_limit_100_000() {
        it_works(50_000);
    }
    #[test]
    fn it_works_limit_1_000_000() {
        it_works(500_000);
    }
    // TODO: These tests overflow the stack on my machine. Fix memory usage.
    #[test]
    fn it_works_limit_10_000_000() {
        it_works(5_000_000);
    }
    #[test]
    fn it_works_limit_100_000_000() {
        it_works(50_000_000);
    }
    //#endregion

    fn it_works(wheel_size: usize) {
        let mut sieve = WheelSieve::new(wheel_size);
        sieve.run();
        if let Some(is_valid) = sieve.validate() {
            assert!(is_valid);
        } else {
            assert!(false);
        }
    }
}
