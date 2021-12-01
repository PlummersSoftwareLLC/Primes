use std::cmp::min;
#[cfg(test)]
use std::collections::HashMap;

/// ## Wheel Sieve
///
/// > Implements the wheel sieving algorithm for generating primes.
///
/// This algorithm deviates quite strongly from the standard Eratosthenes sieve.
/// It relies on building data structures (the _wheels_) that minimize the number of sieving passes required.
///
/// * **Reference paper:** [P. Pritchard - Explaining the wheel sieve](https://www.semanticscholar.org/paper/Explaining-the-wheel-sieve-Pritchard/675801e3b109441cb59e6a368181cfc4a6f84519)
/// * **Compact explanation:** [StackExchange Response](https://math.stackexchange.com/a/3014018/951785)
///
/// #### Implementation peculiarities:
///
/// * **iterators vs loops:** The use of iterators or regular loops is not consistent.
///   In some cases iterators perform better and in others loops do better.
pub struct WheelSieve {
    /// ### Wheel
    ///
    /// > Base data structure for storing wheels/prime numbers.
    ///
    /// #### Usage during the algorithm
    ///
    /// The array represents a set of _marked/unmarked_ numbers.
    /// A number is _marked_ if the corresponding element in the array is true, otherwise it is _unmarked_.
    /// For performance, the same base array is used to represent different things on different phases:
    ///
    /// * During the wheel-generation phase, marked numbers are those included in the wheel.
    /// * During the sieving phase, unmarked numbers are those guaranteed to not be prime.
    /// * After the sieving phase, marked numbers are the prime numbers.
    ///
    /// For performance reasons, the base array doesn't store even numbers.
    /// The `i`th element in the array represents the value `(i * 2) + 1`.
    /// The size of the wheel must therefore be half of the limit.
    wheel: Vec<bool>,
}

impl WheelSieve {
    /// ### New wheel sieve
    ///
    /// > Wheel sieve default constructor.
    ///
    /// Initializes the wheel sieve struct with `true` values.
    pub fn new(wheel_size: usize) -> Self {
        WheelSieve {
            wheel: vec![true; wheel_size],
        }
    }

    /// ### Run sieve wheel
    ///
    /// > Runs the wheel sieving algorithm.
    ///
    /// When done, the `wheel` data structure marks all primes in range.
    pub fn run(&mut self) {
        let max_wheel_size = self.wheel.len();

        //#region ===== Variables and initial values. =====
        let mut curr_wheel_size: usize = 1;
        let mut next_wheel_size: usize;
        let mut next_prime_value: usize = 3;
        let mut next_prime_index: usize = 1;
        let mut wheel_primes: Vec<usize> = vec![];
        //#endregion

        //#region ===== Generate wheels. =====
        while curr_wheel_size < max_wheel_size {
            // Roll the wheel.
            next_wheel_size = next_prime_value * curr_wheel_size;
            let mut insert_index = curr_wheel_size;
            let mut available_space = max_wheel_size - insert_index;
            for _ in 0..(next_prime_value - 1) {
                if curr_wheel_size > available_space {
                    break;
                }
                self.wheel.copy_within(0..curr_wheel_size, insert_index);
                insert_index += curr_wheel_size;
                available_space -= curr_wheel_size;
            }
            // Roll surplus (in case the wheel size exceeded the limit).
            self.wheel.copy_within(0..available_space, insert_index);
            // Sieve prime from the wheel.
            let iter_end = min(next_wheel_size, max_wheel_size) - 1;
            self.wheel[next_prime_index..iter_end]
                .iter_mut()
                .step_by(next_prime_value)
                .for_each(|x| *x = false);

            // Update values.
            wheel_primes.push(next_prime_index);
            while !self.wheel[next_prime_index] {
                next_prime_index += 1;
            }
            next_prime_value = (2 * next_prime_index) + 1;
            curr_wheel_size = next_wheel_size;
        }
        // Mark primes used to generate wheels.
        wheel_primes.iter().for_each(|p| self.wheel[*p] = true);
        //#endregion

        //#region ===== Sieve wheel. =====
        let sieving_limit = ((max_wheel_size * 2) as f32).sqrt() as usize;
        while next_prime_value < sieving_limit {
            // Find next prime.
            while !self.wheel[next_prime_index] {
                next_prime_index += 1;
            }
            next_prime_value = (2 * next_prime_index) + 1;

            // Sieve prime.
            let iter_start = (next_prime_value * next_prime_value) / 2;
            if iter_start < max_wheel_size {
                self.wheel[iter_start..(max_wheel_size - 1)]
                    .iter_mut()
                    .step_by(next_prime_value)
                    .for_each(|x| *x = false);
            }
            next_prime_index += 1;
        }
        //#endregion
    }

    /// ### Validate Results
    ///
    /// > Compare the number of primes found with known values.
    ///
    /// This method is used for testing correctness of implementation.
    #[cfg(test)]
    pub fn validate(self) -> Option<bool> {
        // Known number of primes for specific wheel sizes.
        let mut known_num_primes = HashMap::with_capacity(8);
        known_num_primes.insert(5, 4);
        known_num_primes.insert(50, 25);
        known_num_primes.insert(500, 168);
        known_num_primes.insert(5_000, 1_229);
        known_num_primes.insert(50_000, 9_592);
        known_num_primes.insert(500_000, 78_498);
        known_num_primes.insert(5_000_000, 664_579);
        known_num_primes.insert(50_000_000, 5_761_455);

        // Count marked primes in wheel.
        let num_primes = self.wheel.iter().cloned().filter(|x| *x).count();

        // Compare with known number of primes.
        if let Some(&expected) = known_num_primes.get(&self.wheel.len()) {
            Some(num_primes == expected)
        } else {
            None
        }
    }
}
