//! Multi-threaded sieving of tiles.

use super::{calculate_batch_size, calculate_block_offset, Algorithm};
use crate::sieve::{FlagDataExecute, Sieve, SieveExecute};
use crate::DataType;

use rayon::prelude::*;

/// Marker for tiled execution. Contains the working set size in bytes.
///
/// This algorithm works by first finding all primes up to the square root of the sieve size (and
/// sieving that part of the sieve) with a single thread. Then, the rest of the sieve is split into
/// work units capped by the provided working set size. For each of this units, one thread sieves
/// all found primes.
///
/// This algorithm has some interesting properties. At infinite cores, its performance is mostly
/// constrained by searching for the first primes. After that, each thread keeps working on the same
/// set of memory, providing very high data locality. A working set no larger than the L1 data cache
/// of a CPU core is essential for optimal performance.
///
/// In general, the algorithm should provide near-linear scaling with cores, provided a big enough
/// working set and sieve size. In practice, the linear portion of the algorithm, the overhead for
/// each work unit, a working size that doesn't fit into the L1 data cache and suboptimal division
/// of work between threads can greatly hamper its performance.
#[derive(Clone, Copy)]
pub struct Tile(pub usize);

impl Algorithm for Tile {
    const ID_STR: &'static str = "tile";
}

impl<F: FlagDataExecute<D>, D: DataType> SieveExecute<Tile> for Sieve<Tile, F, D> {
    fn sieve(&mut self) {
        let sqrt = (self.size as f64).sqrt() as usize;
        let cutoff = ((sqrt + 1) / 2 * F::FLAG_SIZE + F::BITS - 1) / F::BITS;

        // first part: get the primes that have to be checked
        let primes = get_primes(&mut self.data, cutoff, sqrt);
        let batch_size = calculate_batch_size::<D>(
            self.data.slice().len() - cutoff,
            self.algorithm.0 * 8 / F::BITS, // limit size to parameter
        );

        // second part: tiled sieving in parallel
        self.data.slice()[cutoff..]
            .par_chunks_mut(batch_size)
            .into_par_iter()
            .enumerate()
            .for_each(|(i, slice)| {
                // flag data initialisation
                for n in slice.iter_mut() {
                    *n = F::INIT_VALUE;
                }

                // main loop
                let offset = (cutoff + i * batch_size) * (F::BITS / F::FLAG_SIZE);
                for prime in &primes {
                    let start_index = calculate_block_offset(prime * prime / 2, offset, *prime);
                    F::fall_through(slice, start_index, *prime);
                }
            });

        self.sieved = true;
    }

    fn thread_count(&self) -> usize {
        let data_size =
            ((self.data.flag_count() * F::FLAG_SIZE + F::BITS - 1) / F::BITS).max(64 * 8 / F::BITS);
        let cutoff = (((self.size as f64).sqrt() as usize / 2 * F::FLAG_SIZE + F::BITS - 1)
            / F::BITS)
            .max(64 * 8 / F::BITS);
        let batch_size = calculate_batch_size::<D>(data_size - cutoff, usize::MAX);

        println!("Batch size {}", batch_size);

        std::cmp::min(
            (data_size - cutoff + batch_size - 1) / batch_size,
            rayon::current_num_threads(),
        )
    }
}

fn get_primes<F: FlagDataExecute<D>, D: DataType>(
    data: &mut F,
    cutoff: usize,
    sqrt: usize,
) -> Vec<usize> {
    for n in data.slice()[..cutoff].iter_mut() {
        *n = F::INIT_VALUE;
    }

    let data_size = cutoff * F::BITS / F::FLAG_SIZE;
    let inner_sqrt = ((data_size * 2 + 1) as f64).sqrt() as usize;
    let mut primes = Vec::with_capacity(sqrt / 2);
    let mut bit = 1;
    let mut prime = 3;

    while prime <= inner_sqrt {
        let start_index = prime * prime / 2;

        F::fall_through(&mut data.slice()[..cutoff], start_index, prime);
        primes.push(prime);

        bit = (bit + 1..data_size).find(|n| data.is_prime(*n)).unwrap();
        prime = bit * 2 + 1;
    }

    primes.push(prime);
    for n in (bit + 1..=(sqrt - 1) / 2).filter(|n| data.is_prime(*n)) {
        let prime = n * 2 + 1;
        primes.push(prime);
    }

    primes
}
