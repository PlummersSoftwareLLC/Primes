//! Dependency-free command-line parsing and execution budgets.

use std::time::Duration;

use super::SieveSize;

pub const HELP: &str = "CokieMiner sieve — zero dependencies\n\
Usage: cokieminer-sieve [--limit N] [--seconds S] [--threads N|auto] [--check]\n\
  --limit N       Inclusive upper limit (default: 1000000)\n\
  --seconds S     Positive run duration in seconds (default: 5)\n\
  --threads N     Independent workers using std::thread (default: 1)\n\
  --threads auto  Use std::thread::available_parallelism()\n\
  --check         Compute one sieve and print the prime count\n\
  --help, -h      Print this help\n\
The official race uses --limit 1000000 --seconds 5.\n\
Shorter runs and other limits are local experiments.";

#[derive(Clone, Copy, Debug)]
pub struct Config {
    pub size: SieveSize,
    pub duration: Duration,
    pub threads: usize,
    pub check: bool,
}

impl Config {
    pub fn parse(arguments: impl IntoIterator<Item = String>) -> Result<Option<Self>, String> {
        let mut limit = None;
        let mut duration = Duration::from_secs(5);
        let mut threads = 1;
        let mut check = false;
        let mut arguments = arguments.into_iter();
        while let Some(argument) = arguments.next() {
            match argument.as_str() {
                "--help" | "-h" => return Ok(None),
                "--check" => check = true,
                "--limit" | "--seconds" | "--threads" => {
                    let value = arguments
                        .next()
                        .ok_or_else(|| format!("missing value for {argument}"))?;
                    match argument.as_str() {
                        "--limit" => {
                            limit =
                                Some(value.parse().map_err(|error| {
                                    format!("invalid limit '{value}': {error}")
                                })?);
                        }
                        "--seconds" => {
                            let seconds = value
                                .parse::<f64>()
                                .map_err(|error| format!("invalid duration '{value}': {error}"))?;
                            duration = Duration::try_from_secs_f64(seconds)
                                .map_err(|error| format!("invalid duration '{value}': {error}"))?;
                            if duration.is_zero() {
                                return Err("duration must be greater than zero".into());
                            }
                        }
                        _ => {
                            threads = if value == "auto" {
                                std::thread::available_parallelism()
                                    .map_err(|error| format!("cannot determine workers: {error}"))?
                                    .get()
                            } else {
                                value.parse().map_err(|error| {
                                    format!("invalid worker count '{value}': {error}")
                                })?
                            };
                            if threads == 0 {
                                return Err("worker count must be greater than zero".into());
                            }
                        }
                    }
                }
                _ => return Err(format!("unknown argument '{argument}'; use --help")),
            }
        }
        let limit = match limit {
            Some(value) => value,
            None => usize::try_from(1_000_000_u32)
                .map_err(|error| format!("default limit does not fit this target: {error}"))?,
        };
        let size = SieveSize::new(limit).map_err(|error| error.to_string())?;
        Ok(Some(Self {
            size,
            duration,
            threads,
            check,
        }))
    }
}
