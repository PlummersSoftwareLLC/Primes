//! The [`Algorithm`] interface, as well as the implementations.

mod serial;
mod stream;
mod tile;

pub use serial::Serial;
pub use stream::Stream;
pub use tile::Tile;

use crate::DataType;

/// Defines an algorithm (and optional execution parameters) for sieve execution.
pub trait Algorithm: Copy {
    /// The identification of the algorithm, used for printing.
    const ID_STR: &'static str;
}

/// Helper method that returns the amount of elements each chunk should contain.
#[inline]
fn calculate_batch_size<D: DataType>(data_len: usize, max_size: usize) -> usize {
    let elements_per_line = (64 * 8 / D::BITS).max(1);

    let thread_align = (data_len + rayon::current_num_threads() - 1) / rayon::current_num_threads();
    let cache_line_align = (thread_align + elements_per_line - 1) & !(elements_per_line - 1);
    cache_line_align.max(elements_per_line).min(max_size)
}

/// Calculates the start offset for a sieve pass for a block that is offset to the start of the
/// sieve.
#[inline]
fn calculate_block_offset(start_index: usize, offset: usize, prime: usize) -> usize {
    if offset <= start_index {
        start_index - offset
    } else {
        // This tells us how far offset is beyond the last flag to be reset before
        // it. Translating between flag indices and actual numbers makes this
        // formula look a bit more complicated than it is.
        let reset_offset = (offset + prime / 2 + 1) % prime;
        if reset_offset == 0 {
            0
        } else {
            prime - reset_offset
        }
    }
}
