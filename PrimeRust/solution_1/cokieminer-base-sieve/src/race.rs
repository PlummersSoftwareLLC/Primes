//! Independent complete passes with a shared start time and private bitmaps.

use std::{
    error::Error,
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

pub fn run(config: &Config) -> Result<RaceResult, Box<dyn Error>> {
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
    let mut sieve = PrimeSieve::new(size);
    passes += 1;
    while start.elapsed() < duration {
        drop(sieve);
        sieve = PrimeSieve::new(size);
        passes += 1;
    }
    WorkerResult { passes, sieve }
}
