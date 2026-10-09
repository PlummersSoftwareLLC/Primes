//! Independent complete passes with a shared start time and private bitmaps.

use std::{
    hint::black_box,
    thread,
    time::{Duration, Instant},
};

use super::{Config, PrimeSieve, SieveSize};

pub struct RaceResult {
    pub passes: u128,
    pub elapsed: Duration,
    pub workers: Vec<WorkerResult>,
}

pub struct WorkerResult {
    pub passes: u64,
    pub sieve: PrimeSieve,
}

pub fn run(config: &Config) -> Result<RaceResult, Box<dyn std::error::Error>> {
    if config.threads == 1 {
        let start = Instant::now();
        let worker = run_worker(config.size, start, config.duration);
        let elapsed = start.elapsed();
        return Ok(RaceResult {
            passes: u128::from(worker.passes),
            elapsed,
            workers: vec![worker],
        });
    }

    let mut handles = Vec::with_capacity(config.threads);
    let mut workers = Vec::with_capacity(config.threads);
    let size = config.size;
    let duration = config.duration;
    let start = Instant::now();
    for _ in 0..config.threads {
        handles.push(thread::spawn(move || run_worker(size, start, duration)));
    }
    let mut passes = 0_u128;
    for handle in handles {
        let worker = handle.join().map_err(|_| "a sieve worker panicked")?;
        // There are at most usize::MAX <= 2^64-1 workers on the supported
        // pointer widths. Each contributes at most u64::MAX, so the sum
        // is bounded by (2^64-1)^2 < 2^128 and cannot overflow.
        passes += u128::from(worker.passes);
        workers.push(worker);
    }
    let elapsed = start.elapsed();
    Ok(RaceResult {
        passes,
        elapsed,
        workers,
    })
}

fn run_worker(size: SieveSize, start: Instant, duration: Duration) -> WorkerResult {
    let mut passes = 0_u64;
    loop {
        // Escape the full bitmap on every pass. Allocation and the entire
        // sieve remain observable, even with whole-program optimization.
        let sieve = black_box(PrimeSieve::new(size));
        passes += 1;
        if start.elapsed() >= duration {
            return WorkerResult { passes, sieve };
        }
    }
}
