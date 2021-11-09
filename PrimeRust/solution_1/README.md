# Rust solution by @mike-barber

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Parallelism](https://img.shields.io/badge/Parallel-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Bit count](https://img.shields.io/badge/Bits-8-yellowgreen)

Contributors:
- Michael Barber @mike-barber https://www.github.com/mike-barber -- original author
- Kai Rese @Kulasko https://github.com/Kulasko -- numerous idiomatic improvements and detailed code review
- @GordonBGood -- for plenty of collaboration on the `unrolled-hybrid` and `extreme-hybrid` solutions; check out his solutions, including Nim, Chapel, Haskell and Julia.

This is a somewhat Rust-idiomatic version that has the storage of prime flags abstracted out, with the prime sieve algorithm generic over the storage. Two kinds of storage are implemented:
    
- `bit storage` is equivalent to the above C++ `vector<bool>` case and uses individual bits within a byte to store true/false. I have three variations of this implemented, with comments in the code.
- `byte storage` has a simple vector of bytes, and just has `0 == false` and `1 == true`. It's a lot faster. But only for small datasets, since it uses more memory.
- this version still intends to be similar to the C++ implementations so it's easier to compare; it's not fully idiomatic Rust, and is not intended to be.
- it runs both single-thread and multi-thread tests.

Note that C++'s `vector<bool>` is a controversial specialisation of `vector` that `*may*` use more efficient storage than using a whole byte for 1 or 0. It's up to the compiler vendor, and is not standardised. Typically it's using individual bits for storage, within bytes or words. So the `bit storage` I have implemented closely resembles this. Rust has no such thing built in, although there are several crates available. For clarity, it's implemented manually in this solution.

Several variants of the `bit storage` approach exist, and they differ in layout (striped vs linear) or in the algorithm we use to traverse through the storage and reset the bits. The variants are explained below.

## Striped storage

My `striped` variations have proved quite popular, and I'm very pleased to see that the approach has been successfully adopted in a number of other solutions. Given this popularity, it's worth writing a few notes about how they work.

The original `striped` storage algorithm resulted from a hunch I had about the relative cost of creating a mask compared with the cost of applying it. I suspected that the cost of creating the mask, along with the cost of figuring out which address to apply it to, was quite high. So the `striped` storage solution took a slightly different approach to the storage. In the normal, linear, case, we store bits like the following. Pretend we have just two bytes of storage.

```
word0: [0   1   2   3   4   5   6   7], 
word1: [8   9  10  11  12  13  14  15], 
```

In the `striped` storage case, we essentially _transpose_ this layout like this:

```
word0: [0   2   4   6   8  10  12  14]
word1: [1   3   5   7   9  11  13  15]
```

This allows us to set a single mask for the first bit, then apply it with very regular `skip` factors to _all_ the words. Then proceed to calculate the mask for the second bit, and apply it to all the words again. This works really well, but it's not perfect: we're looping over all the words multiple times, and this has poor _locality_: we land up thrashing the cache, particularly on smaller processors.

This brings us to the later algorithm, `striped-blocks`, that addresses this locality problem by splitting the problem into smaller chunks (blocks) of 16kB or 4kB. Instead of looping over all the words in the sieve and thrashing the cache, we apply the masks for each bit in turn to the first block. Once we're done with the first block, we proceed to the next. The compiler may be able to optimise it slightly better with known block sizes too, but I'm not sure if this is the case.

Most recently, I've added an optional feature called `HYBRID` to `striped-blocks` that has a specific algorithm for resetting bits when we have smaller skip factors. The motivation for this is that we're typically touching every word in the block with small skip factors, and typically having to reset multiple bits in each word. We're not allowed to reset multiple bits in one operation, but we're definitely able to apply multiple (separate, single-bit) masks to a single word before proceeding to the next word. 

This variation calculates the masks that will need to be applied: 8 of them, one for each bit, for each index. We have `SKIP` indices we need to keep track of, as the pattern repeats. So we can calculate all these masks before we loop through the words, then apply them in a very mechanical way with very little calculation required. Because the method is generic over the `SKIP` factor, the compiler can be quite aggressive with the loop optimisation. This seems to result in a 10-20% improvement in performance. And I think it's quite a fun application of const generics in Rust. 

Part of the inspiration for this hybrid algorithm comes from the work of two people, whom I gratefully acknowledge here: 

- @ItalyToast for the novel approach to resetting bits in a dense way in the traditional linear sieve in `PrimeCSharp/solution_4`. It got me thinking about how I could potentially apply a similar technique to the `striped-blocks`, although it required a bit of thinking.
- @GordonBGood for his really interesting `PrimeNim/solution_3` implementation. I definitely had a good think about how I could apply a similar code-gen approach in Rust, and there's some interesting stuff I could potentially do with procedural macros. Before trying that, I decided to see what I could do with const generics and calculating masks, and that path led me to the hybrid algorithm. I've also implemented the extreme algorithm that is a lot more similar to Gordon's Nim solution.

## Unrolled hybrid storage

This was designed in a very fun collaboration with @GordonBGood. It has standard linear bit storage, unlike the transposed `striped` case. But it uses something similar to the dense resetting in the `striped`, `hybrid` solution. And it also uses the specialised sparse reset functions with repeating patterns and addresses from @GordonBGood's solutions. 

However, rather than generate all the code directly, I am able to use Rust's const generics to specialise the reset functions. This perhaps a little harder to understand in Rust, since you need to understand how the generics are used to compile down to constants: each new type created with a given generic value, e.g. N=2, results in specific code corresponding to that type. This allows the compiler to treat N as a constant in that context, and perform optimisations. Specifically, we are able to generate a pattern of *single bit* masks that repeat. The compiler is able to then generate `OR` mask instructions with immediate (literal) values, rather than needing to obtain the mask from a register or memory location. Have a look at Gordon's Chapel solution: he used a code-generator to render similar functions, and it's quite easy to see the algorithm there, as he used a code generator to write the dense reset functions. Rust is doing something similar, but in the background via generics.

Now, in order to dispatch to one of these specific functions (remember `N` is a literal constant at compile time), we either need to write a big `match` statement, or we need to generate it. Doing it by hand is feasible -- there are only 32 dense resetters, and 8 sparse ones. But it's no fun doing it by hand. Although it's way more code, Rust's procedural macros are interesting, so I decided to take that approach: essentially, we tell the macro what the range of numbers is, and which functions to call, and it writes the `match` statement for us. I do some substitution of the identifier `N` to a literal value in the `TokenStream` processing. It's a nice way for Rust to write Rust at compile time. It's a gratuitously large hammer for a small nail, purely here to pique people's interest.

## Extreme hybrid storage

This is functionally-equivalent to the unrolled hybrid storage explained above, but the code is written using a different technique: we use a procedural macro to write the reset functions _explicitly_. There appears to be a very slight performance gain, at least on AMD hardware, perhaps due to giving LLVM less work to do than in the above case. It's included mostly for completeness as it is conceptually identical to @GordonBGood's extreme implementations, and it's easier to see the relationship between our various solutions with a common approach employed.

The `extreme_reset` macro generates specific functions to reset each skip factor. Each 64-bit word is loaded from memory, then all the single bit masks are applied to it one by one, and finally the resulting value is written back to memory. Part of produced code for the first `skip` factor of 3 is reproduced below. Pardon the formatting: the procedural macro does not bother formatting code in a particularly pretty way.

```rust
fn extreme_reset_003(words : & mut [u64])
{
    debug_assert!
    (4usize < words.len() * 64,
     "square_start should be within the bounds of our array; check caller") ;
    let mut chunks = words [0usize ..].chunks_exact_mut(3usize) ;
    (& mut
     chunks).for_each(| chunk |
                      {
                          unsafe
                          {
                              let mut word = * chunk.get_unchecked(0usize) ;
                              word |= 2u64 ; word |= 16u64 ; word |= 128u64 ;
                              word |= 1024u64 ; word |= 8192u64 ; word |=
                              65536u64 ; word |= 524288u64 ; word |=
                              4194304u64 ; word |= 33554432u64 ; word |=
                              268435456u64 ; word |= 2147483648u64 ; word |=
                              17179869184u64 ; word |= 137438953472u64 ; word
                              |= 1099511627776u64 ; word |= 8796093022208u64 ;
                              word |= 70368744177664u64 ; word |=
                              562949953421312u64 ; word |= 4503599627370496u64
                              ; word |= 36028797018963968u64 ; word |=
                              288230376151711744u64 ; word |=
                              2305843009213693952u64 ; *
                              chunk.get_unchecked_mut(0usize) = word ;
                          }
                          // etc for each word in the chunk.
```

## Run instructions

- using Docker is the easiest way to get started without installing Rust:
    - `./build-docker.sh`
    - `./run-docker.sh`
    - `./run-docker.sh --help` for help with command line options
- `cargo run --release` if you've got Rust installed

There are more notes for getting started with Rust at the bottom, under `Quick start for those interested in Rust`

## Output

Tested on a Ryzen 3900X, Rust 1.53, running on WSL2.

This is as reported on `stdout`:
```
mike-barber_byte-storage;18661;5.0000872612;1;algorithm=base,faithful=yes,bits=8
mike-barber_bit-storage;12098;5.0002989769;1;algorithm=base,faithful=yes,bits=1
mike-barber_bit-storage-rotate;14090;5.0002450943;1;algorithm=base,faithful=yes,bits=1
mike-barber_bit-storage-striped;19205;5.0002722740;1;algorithm=base,faithful=yes,bits=1
mike-barber_bit-storage-striped-blocks;21305;5.0001897812;1;algorithm=base,faithful=yes,bits=1
mike-barber_bit-storage-striped-blocks-small;20338;5.0000920296;1;algorithm=base,faithful=yes,bits=1
mike-barber_byte-storage;170552;5.0009374619;24;algorithm=base,faithful=yes,bits=8
mike-barber_bit-storage;134112;5.0011000633;24;algorithm=base,faithful=yes,bits=1
mike-barber_bit-storage-rotate;164057;5.0006699562;24;algorithm=base,faithful=yes,bits=1
mike-barber_bit-storage-striped;191961;5.0007262230;24;algorithm=base,faithful=yes,bits=1
mike-barber_bit-storage-striped-blocks;221099;5.0007972717;24;algorithm=base,faithful=yes,bits=1
mike-barber_bit-storage-striped-blocks-small;224842;5.0006871223;24;algorithm=base,faithful=yes,bits=1
```

We report more informative metrics to `stderr` too, but these don't go into the report, as recorded below.

## Performance - **Ryzen 3900X**

Tested on a Ryzen 3900X, Rust 1.51, running on WSL2.
- Examples are run with `cargo run --release -- --repetitions=3` to get 3 repetitions.

```
Computing primes to 1000000 on 1 thread for 5 seconds.
byte-storage    Passes: 18194, Threads: 1, Time: 5.0000767708, Average: 0.0002748201, Limit: 1000000, Counts: 78498, Valid: Pass
byte-storage    Passes: 18175, Threads: 1, Time: 5.0002570152, Average: 0.0002751173, Limit: 1000000, Counts: 78498, Valid: Pass
byte-storage    Passes: 18050, Threads: 1, Time: 5.0000729561, Average: 0.0002770124, Limit: 1000000, Counts: 78498, Valid: Pass

Computing primes to 1000000 on 1 thread for 5 seconds.
bit-storage     Passes: 11917, Threads: 1, Time: 5.0003094673, Average: 0.0004195946, Limit: 1000000, Counts: 78498, Valid: Pass
bit-storage     Passes: 11941, Threads: 1, Time: 5.0002613068, Average: 0.0004187473, Limit: 1000000, Counts: 78498, Valid: Pass
bit-storage     Passes: 11961, Threads: 1, Time: 5.0001654625, Average: 0.0004180391, Limit: 1000000, Counts: 78498, Valid: Pass

Computing primes to 1000000 on 24 threads for 5 seconds.
byte-storage    Passes: 169821, Threads: 24, Time: 5.0006589890, Average: 0.0000294466, Limit: 1000000, Counts: 78498, Valid: Pass
byte-storage    Passes: 169607, Threads: 24, Time: 5.0007286072, Average: 0.0000294842, Limit: 1000000, Counts: 78498, Valid: Pass
byte-storage    Passes: 169333, Threads: 24, Time: 5.0009894371, Average: 0.0000295335, Limit: 1000000, Counts: 78498, Valid: Pass

Computing primes to 1000000 on 24 threads for 5 seconds.
bit-storage     Passes: 151804, Threads: 24, Time: 5.0008311272, Average: 0.0000329427, Limit: 1000000, Counts: 78498, Valid: Pass
bit-storage     Passes: 151710, Threads: 24, Time: 5.0007929802, Average: 0.0000329628, Limit: 1000000, Counts: 78498, Valid: Pass
bit-storage     Passes: 151644, Threads: 24, Time: 5.0008230209, Average: 0.0000329774, Limit: 1000000, Counts: 78498, Valid: Pass
```

## Performance - **Raspberry Pi 4**

Tested on a **Raspberry Pi 4**, standard clock with active cooling, running 64-bit Ubuntu Server 20.04.
- Examples are run with `cargo run --release -- --repetitions=3` to get 3 repetitions.
- Docker works on the Pi too :)

```
Computing primes to 1000000 on 1 thread for 5 seconds.
byte-storage    Passes: 2325, Threads: 1, Time: 5.0005192757, Average: 0.0021507610, Limit: 1000000, Counts: 78498, Valid: Pass
byte-storage    Passes: 2332, Threads: 1, Time: 5.0009851456, Average: 0.0021445048, Limit: 1000000, Counts: 78498, Valid: Pass
byte-storage    Passes: 2334, Threads: 1, Time: 5.0021271706, Average: 0.0021431565, Limit: 1000000, Counts: 78498, Valid: Pass

Computing primes to 1000000 on 1 thread for 5 seconds.
bit-storage     Passes: 2434, Threads: 1, Time: 5.0012273788, Average: 0.0020547360, Limit: 1000000, Counts: 78498, Valid: Pass
bit-storage     Passes: 2433, Threads: 1, Time: 5.0001516342, Average: 0.0020551383, Limit: 1000000, Counts: 78498, Valid: Pass
bit-storage     Passes: 2434, Threads: 1, Time: 5.0013856888, Average: 0.0020548010, Limit: 1000000, Counts: 78498, Valid: Pass

Computing primes to 1000000 on 4 threads for 5 seconds.
byte-storage    Passes: 393, Threads: 4, Time: 5.0251092911, Average: 0.0127865374, Limit: 1000000, Counts: 78498, Valid: Pass
byte-storage    Passes: 398, Threads: 4, Time: 5.0315208435, Average: 0.0126420120, Limit: 1000000, Counts: 78498, Valid: Pass
byte-storage    Passes: 397, Threads: 4, Time: 5.0280776024, Average: 0.0126651833, Limit: 1000000, Counts: 78498, Valid: Pass

Computing primes to 1000000 on 4 threads for 5 seconds.
bit-storage     Passes: 8083, Threads: 4, Time: 5.0011849403, Average: 0.0006187288, Limit: 1000000, Counts: 78498, Valid: Pass
bit-storage     Passes: 8062, Threads: 4, Time: 5.0017576218, Average: 0.0006204115, Limit: 1000000, Counts: 78498, Valid: Pass
bit-storage     Passes: 8358, Threads: 4, Time: 5.0018110275, Average: 0.0005984459, Limit: 1000000, Counts: 78498, Valid: Pass
```

Some interesting things to note here. The little Pi only has 1MB of cache, total. We're blowing that cache immediately using `byte` level storage when running on multiple threads: it's just thrashing the cache with a large dataset. However, the `bit` level storage is more efficient and data for all threads fits in the cache in this case and we get a decent performance uplift.

I'm expecting the Apple M1 to do a lot better given it has 16MB of cache, but I can't test this. Fundamentally, you'll see the same thing on any processor once the dataset doesn't fit inside the cache, essentially where `sieve_size * threads > cache`. So for small datasets, `byte` level storage may be faster given there's less work to do. For larger datasets, you need the memory efficiency instead.

# Configuration

I've enabled all the optimisation I'm aware of, including:
- setting `target-cpu=native`
- link time optimisation and `codegen-units=1`
- pay attention to [.cargo/config](.cargo/config) if you want to run it on, say, Mac. I don't have one.

# Quick start for those interested in Rust

Install Rust. It's really easy: https://www.rust-lang.org/learn/get-started. On Linux, it's just `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`; probably the same or similar on Mac. On Windows, just grab the installer from the link.

There is a convenient [run.sh](run.sh) file that builds and runs the solution.

Alternately, in the directory containing this README file, 
- run tests: `cargo test` 
- build: `cargo build --release`
- run: `target/release/prime-sieve-rust` or `cargo run --release`
    - for help with command line parameters: `target/release/prime-sieve-rust --help`
    - this allows you to specify sieve size, threads, etc.

It'll take a little while to compile the first time. Because Rust. It's doing a fair bit of work :)

To play with the code, the simplest approach is to use *Visual Studio Code* and install the `rust-analyzer` plugin.

And if you want to learn more, there are tons of great resources, including a good introductory book, on https://www.rust-lang.org/learn/get-started

## Docker

You can also run the solution using Docker, without installing Rust.

```
./build-docker.sh
./run-docker.sh
```

For available command line options, just do `./run-docker.sh --help`
