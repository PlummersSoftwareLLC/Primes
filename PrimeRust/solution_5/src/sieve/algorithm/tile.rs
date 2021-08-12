use super::{calculate_batch_size, calculate_block_offset, Algorithm};
use crate::sieve::{FlagDataExecute, Sieve, SieveBase, SieveExecute};
use crate::DataType;

use rayon::prelude::*;

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
            self.algorithm.0 * 8 / F::BITS,
        );

        self.data.slice()[cutoff..]
            .par_chunks_mut(batch_size)
            .into_par_iter()
            .enumerate()
            .for_each(|(i, slice)| {
                for n in slice.iter_mut() {
                    *n = F::INIT_VALUE;
                }

                let offset = (cutoff + i * batch_size) * (F::BITS / F::FLAG_SIZE);
                for prime in &primes {
                    let start_index = calculate_block_offset(prime * prime / 2, offset, *prime);
                    F::fall_through(slice, start_index, *prime);
                }
            });

        self.sieved = true;
    }

    fn thread_count(&self) -> usize {
        std::cmp::min(
            (self.size + 1) / 2 * Self::FLAG_SIZE / 64,
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

        bit = (bit + 1..data_size)
            .find(|n| data.is_prime(*n))
            .unwrap();
        prime = bit * 2 + 1;
    }

    primes.push(prime);
    for n in (bit + 1..=(sqrt - 1) / 2).filter(|n| data.is_prime(*n)) {
        let prime = n * 2 + 1;
        primes.push(prime);
    }

    primes
}
