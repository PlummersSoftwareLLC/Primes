//! Single threaded execution. Not used for benching.

use super::Algorithm;
use crate::sieve::{FlagDataExecute, Sieve, SieveExecute};
use crate::DataType;

/// Marker for serial execution.
///
/// As per discussion with the maintainers of the competition repository, this isn't used as a
/// bench. However, it still exists inside the project for testing purposes.
#[derive(Clone, Copy)]
pub struct Serial;

impl Algorithm for Serial {
    const ID_STR: &'static str = "serial";
}

impl<F: FlagDataExecute<D>, D: DataType> SieveExecute<Serial> for Sieve<Serial, F, D> {
    #[inline]
    fn sieve(&mut self) {
        // flag data initialization
        for n in self.data.slice() {
            *n = F::INIT_VALUE;
        }

        // main loop
        let sqrt = (self.size as f64).sqrt() as usize;
        let mut prime = 3;

        while prime <= sqrt {
            F::fall_through(self.data.slice(), prime * prime / 2, prime);

            prime = (prime / 2 + 1..self.size / 2)
                .find(|n| self.data.is_prime(*n))
                .unwrap()
                * 2
                + 1;
        }

        self.sieved = true;
    }

    fn thread_count(&self) -> usize {
        1
    }
}
