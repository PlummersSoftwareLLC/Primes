//! Command-line runner for the Primes software drag race.

use std::process::ExitCode;

use cokieminer_sieve::{PrimeSieve, SieveSize};

mod cli;
mod race;

use cli::{Config, HELP};
use race::run;

fn main() -> ExitCode {
    match execute() {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("error: {error}");
            ExitCode::FAILURE
        }
    }
}

fn execute() -> Result<(), Box<dyn std::error::Error>> {
    let Some(config) = Config::parse(std::env::args().skip(1))? else {
        println!("{HELP}");
        return Ok(());
    };
    if config.check {
        let sieve = PrimeSieve::new(config.size);
        let count = sieve.count_primes();
        validate(config.size.limit(), count)?;
        println!("limit={}, count={count}", config.size.limit());
        return Ok(());
    }

    let result = run(&config)?;
    let mut count = None;
    for worker in &result.workers {
        let actual = worker.sieve.count_primes();
        validate(config.size.limit(), actual)?;
        if count.is_some_and(|previous| previous != actual) {
            return Err("worker results disagree".into());
        }
        count = Some(actual);
    }
    let count = count.ok_or("no worker returned a complete sieve")?;
    let validation = if known_count(config.size.limit()).is_some() {
        "yes"
    } else {
        "no reference count"
    };
    eprintln!(
        "limit={}, primes={count}, bitmap={} bytes, passes={}, time={:.6}s, threads={}, validated={validation}",
        config.size.limit(),
        config.size.bytes(),
        result.passes,
        result.elapsed.as_secs_f64(),
        config.threads,
    );
    println!(
        "CokieMiner_rust_w30;{};{:.9};{};algorithm=wheel,faithful=yes,bits=1",
        result.passes,
        result.elapsed.as_secs_f64(),
        config.threads,
    );
    Ok(())
}

fn validate(limit: usize, actual: usize) -> Result<(), String> {
    if let Some(expected) = known_count(limit)
        && actual != expected
    {
        return Err(format!(
            "invalid sieve at {limit}: expected {expected} primes, found {actual}"
        ));
    }
    Ok(())
}

fn known_count(limit: usize) -> Option<usize> {
    let count: u64 = match u64::try_from(limit).ok()? {
        0 | 1 => Some(0),
        2 => Some(1),
        3 | 4 => Some(2),
        5 | 6 => Some(3),
        7..=10 => Some(4),
        100 => Some(25),
        1_000 => Some(168),
        10_000 => Some(1_229),
        100_000 => Some(9_592),
        1_000_000 => Some(78_498),
        10_000_000 => Some(664_579),
        100_000_000 => Some(5_761_455),
        1_000_000_000 => Some(50_847_534),
        _ => None,
    }?;
    // Every listed pi(n) is <= n, so any count for a representable limit also
    // fits usize, including on 16-bit pointers.
    usize::try_from(count).ok()
}
