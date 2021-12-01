# Julia solution by GordonBGood
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)

This is a "low-level" style implementation in Julia to get as much as speed as possible out of the language. It is **not** designed to be entirely idiomatic Julia code, but it does show the kind of performance that can be extracted from the language.

## Description

Instead of using Julia's native `BitVector` type, this solution manually implements a bit array using a `Vector{UInt8}` which then allows this sieve buffer to be sieved using different techniques:

1. Bit Twiddling, which uses conventional logical shifting and logical `or`'ing/`and`'ing operations.  This is identical to the implementations by others except that it uses a slight optimization that the mask values are taken from a Look Up Table (LUT), which reduces the number of logical operations and makes the inner loop tighter and thus faster.
2. Striding, which recognizes that there is a modulo pattern across bytes where every eighth marking position is at the same bit index, and therefore eight separate simple loops can be uses with a constant bit marking mask for faster culling/marking of composite number bits; this also gives better cache associativity for reduced "cache thrashing" time.
3. Striding by Blocks, which uses the above technique but does the culls in sixteen Kilobyte blocks so that all culls for all eight loops for a given base prime value are within one block before moving on the successive blocks; this reduces "cache thrashing" time even further.
4. The Extreme technique uses a macro to combine the above eight loops into one loop that does all the marking/culling in a single pass (no blocks used), where the constant/immediate mask values are computed by the macro so no "bit twiddling is necessary.  This would reduce the time per marking operation to about one CPU clock cycle each except for the time lost to "cache thrashing".  Although all of the possibilities are generated for and step value, in actual fact culling/marking is only done by odd primes, so modulo eight that leaves just four modulo stepping values, and while all eight possible start index values are in the table, in actual fact the algorithm is always run from the start of the sieve buffer to the end so only one start bit index per base prime value is used, meaning that out of the total combination of 64 cases, only four are used.
5. Finally, the Extreme Hybrid technique is the same as the above Extreme technique for larger base prime values above thirteen but uses an alternate dense marking technique for smaller base prime values below that threshold, which reads a given 64-bit word into a variable/register, marks all values within that word on a bit-by-bit operation basis, then commits the contents of that given 64-bit word back to its original location.  In other languages, this is effective up to base prime value thresholds of 31 and more, but in Julia it was found that the optimum dense base prime value threshold is 13; this is likely due to Julia's dynamic nature.  As well, using very large thresholds means that the macro uses for generating the constants using for the "dense" technique takes a very long time to compile and is likely very difficult for the compiler to optimize due to its large generated lines of code, so perhaps explains why further gains above this threshold don't seem to work.

Since Julia has a "warm up time" each test is run twice meaning each technique takes ten seconds instead of only five, but with only the second run reported as to test results.  This means that the initial compile and tuning phases are removed from the final timing results.

This implementation only stores bits for the odd numbers above 3 in an "inverted" bit array, i.e., bits are set when the number is *not prime* and bits are unset when the number is *prime*. This simplifies the set_bit operation slightly (`arr[i] |= mask vs. arr[i] &= ~mask`). All techniques only use the optimizations of only testing for base prime values up to the square root of the total range to be sieved, staring culling at the square of that base value, and use one operation per single bit marking/culling, so is `faithful to base`.

## Run instructions

### Running locally

First, make sure that you have installed [Julia 1.5 or newer](https://julialang.org/downloads/) and have verified that your installation works.

To build and run the solution locally, run the following command:
```
julia -O3 primes.jl
```

### Running in Docker

If you want to run this solution in a Docker container, follow the steps below.

1. Build or update the Docker image. You can skip this step if you have
   already built the `primejulia-4` image.

```
docker build --pull -t primejulia-4 .
```

2. Run the `primejulia-4` image as a container.
```
docker run -it --rm primejulia-4
```
3. If you want to remove the built Docker image, run:
```
docker image rm primejulia-4
```

This solution's [Dockerfile](Dockerfile) uses the `julia:1.6-buster` image to provide maximum support for different architectures, most importantly ARM64 and x86_64. This also helps avoid any possible issues caused by Julia's [Tier 3 support for musl](https://julialang.org/downloads/#currently_supported_platforms), including Alpine Linux.


## Output

This is the output from running the solution on a machine with an Intel Core SkyLake i5-6500 CPU at 3.6 GHz (single threaded turbo):

```
GGordonBGood_bittwiddle;8581;5.000138998031616;1;algorithm=base,faithful=yes,bits=1
GordonBGood_stride8;10834;5.000280141830444;1;algorithm=base,faithful=yes,bits=1
GordonBGood_stride8block16k;13334;5.000041961669922;1;algorithm=base,faithful=yes,bits=1
GordonBGood_extreme;16927;5.000233173370361;1;algorithm=base,faithful=yes,bits=1
GordonBGood_extremehybrid;24669;5.000000953674316;1;algorithm=base,faithful=yes,bits=1
```

When running in Docker on the same machine, the output is as follows:

```
GordonBGood_bittwiddle;8592;5.000308036804199;1;algorithm=base,faithful=yes,bits=1
GordonBGood_stride8;10822;5.000256061553955;1;algorithm=base,faithful=yes,bits=1
GordonBGood_stride8block16k;12951;5.0003461837768555;1;algorithm=base,faithful=yes,bits=1
GordonBGood_extreme;16869;5.000155925750732;1;algorithm=base,faithful=yes,bits=1
GordonBGood_extremehybrid;24068;5.000108957290649;1;algorithm=base,faithful=yes,bits=1
```

```
                                                                Single-threaded                                                                
┌───────┬────────────────┬──────────┬─────────────────────────────┬────────┬──────────┬─────────┬───────────┬──────────┬──────┬───────────────┐
│ Index │ Implementation │ Solution │ Label                       │ Passes │ Duration │ Threads │ Algorithm │ Faithful │ Bits │ Passes/Second │
├───────┼────────────────┼──────────┼─────────────────────────────┼────────┼──────────┼─────────┼───────────┼──────────┼──────┼───────────────┤
│   1   │ julia          │ 4        │ GordonBGood_extremehybrid   │ 24042  │ 5.00020  │    1    │   base    │   yes    │ 1    │  4808.20581   │
│   2   │ julia          │ 4        │ GordonBGood_extreme         │ 16901  │ 5.00001  │    1    │   base    │   yes    │ 1    │  3380.19388   │
│   3   │ julia          │ 4        │ GordonBGood_stride8block16k │ 12989  │ 5.00010  │    1    │   base    │   yes    │ 1    │  2597.74698   │
│   4   │ julia          │ 4        │ GordonBGood_stride8         │ 10685  │ 5.00032  │    1    │   base    │   yes    │ 1    │  2136.86448   │
│   5   │ julia          │ 4        │ GordonBGood_bittwiddle      │  8519  │ 5.00012  │    1    │   base    │   yes    │ 1    │  1703.75767   │
└───────┴────────────────┴──────────┴─────────────────────────────┴────────┴──────────┴─────────┴───────────┴──────────┴──────┴───────────────┘
```

## Implementation Notes for Reviewers

In order to check that this implementation is `faithful to base`, reviewers will likely want to inspect the code generated by the `set_sparse_bits` and `set_dense_bits` macros; this is easily done by uncommenting the code block starting at line 311 by inserting a space between the initial `#` and `=` characters and likely also comment out line 322 by adding a `#` character at the beginning of the line (so that the tests are not run), then compiling the program with `julia -O3 primes.jl > macros.jl` so that the generated code is ouput to a "macro.jl" file.  This generated file can then be inspected to see what the generated code is doing.

For instance, for the base prime value modulo three for which the start bit index will also be three (case 27), the generated code for the eight-cull inner loop is as follows (from the "macros.jl" file, lines 1349 to 1374, inclusive ):

```
        #= /home/Gordon/Documents/JuliaWork/BenchSoE/src/DragRace.jl:61 =#
        begin
            $(Expr(:inbounds, true))
            local var"#239#val" = while var"#210#c" <= var"#211#clmt"
                        #= /home/Gordon/Documents/JuliaWork/BenchSoE/src/DragRace.jl:63 =#
                        tstbv[var"#210#c"] |= 0x08
                        #= /home/Gordon/Documents/JuliaWork/BenchSoE/src/DragRace.jl:64 =#
                        tstbv[var"#210#c" + var"#203#r1"] |= 0x40
                        #= /home/Gordon/Documents/JuliaWork/BenchSoE/src/DragRace.jl:65 =#
                        tstbv[var"#210#c" + var"#204#r2"] |= 0x02
                        #= /home/Gordon/Documents/JuliaWork/BenchSoE/src/DragRace.jl:66 =#
                        tstbv[var"#210#c" + var"#205#r3"] |= 0x10
                        #= /home/Gordon/Documents/JuliaWork/BenchSoE/src/DragRace.jl:67 =#
                        tstbv[var"#210#c" + var"#206#r4"] |= 0x80
                        #= /home/Gordon/Documents/JuliaWork/BenchSoE/src/DragRace.jl:68 =#
                        tstbv[var"#210#c" + var"#207#r5"] |= 0x04
                        #= /home/Gordon/Documents/JuliaWork/BenchSoE/src/DragRace.jl:69 =#
                        tstbv[var"#210#c" + var"#208#r6"] |= 0x20
                        #= /home/Gordon/Documents/JuliaWork/BenchSoE/src/DragRace.jl:70 =#
                        tstbv[var"#210#c" + var"#209#r7"] |= 0x01
                        #= /home/Gordon/Documents/JuliaWork/BenchSoE/src/DragRace.jl:71 =#
                        var"#210#c" += bp
                    end
            $(Expr(:inbounds, :pop))
            var"#239#val"
        end
```

where one can clearly see the eight marking operations by pre-computed constant mask value within this inner loop; this would apply for base prime values such as nineteen, which has modulo eight of three and starts at index value of 179 which has a modulo eight value of three.

For the dense marking, for a real base prime value of three (case zero), the inner marking code loop is as follows ("macros.jl" lines 3181 to 3257):

```
        #= /home/Gordon/Documents/JuliaWork/BenchSoE/src/DragRace.jl:121 =#
        begin
            $(Expr(:inbounds, true))
            local var"#285#val" = while var"#280#wordIndex" <= var"#281#wordLimit"
                        #= /home/Gordon/Documents/JuliaWork/BenchSoE/src/DragRace.jl:123 =#
                        begin
                            var"#282#v" = var"#279#cws"[var"#280#wordIndex" + 0] | 0x0000000000000001
                            var"#282#v" |= 0x0000000000000008
                            var"#282#v" |= 0x0000000000000040
                            var"#282#v" |= 0x0000000000000200
                            var"#282#v" |= 0x0000000000001000
                            var"#282#v" |= 0x0000000000008000
                            var"#282#v" |= 0x0000000000040000
                            var"#282#v" |= 0x0000000000200000
                            var"#282#v" |= 0x0000000001000000
                            var"#282#v" |= 0x0000000008000000
                            var"#282#v" |= 0x0000000040000000
                            var"#282#v" |= 0x0000000200000000
                            var"#282#v" |= 0x0000001000000000
                            var"#282#v" |= 0x0000008000000000
                            var"#282#v" |= 0x0000040000000000
                            var"#282#v" |= 0x0000200000000000
                            var"#282#v" |= 0x0001000000000000
                            var"#282#v" |= 0x0008000000000000
                            var"#282#v" |= 0x0040000000000000
                            var"#282#v" |= 0x0200000000000000
                            var"#282#v" |= 0x1000000000000000
                            var"#279#cws"[var"#280#wordIndex" + 0] |= var"#282#v" | 0x8000000000000000
                            var"#282#v" = var"#279#cws"[var"#280#wordIndex" + 1] | 0x0000000000000004
                            var"#282#v" |= 0x0000000000000020
                            var"#282#v" |= 0x0000000000000100
                            var"#282#v" |= 0x0000000000000800
                            var"#282#v" |= 0x0000000000004000
                            var"#282#v" |= 0x0000000000020000
                            var"#282#v" |= 0x0000000000100000
                            var"#282#v" |= 0x0000000000800000
                            var"#282#v" |= 0x0000000004000000
                            var"#282#v" |= 0x0000000020000000
                            var"#282#v" |= 0x0000000100000000
                            var"#282#v" |= 0x0000000800000000
                            var"#282#v" |= 0x0000004000000000
                            var"#282#v" |= 0x0000020000000000
                            var"#282#v" |= 0x0000100000000000
                            var"#282#v" |= 0x0000800000000000
                            var"#282#v" |= 0x0004000000000000
                            var"#282#v" |= 0x0020000000000000
                            var"#282#v" |= 0x0100000000000000
                            var"#282#v" |= 0x0800000000000000
                            var"#279#cws"[var"#280#wordIndex" + 1] |= var"#282#v" | 0x4000000000000000
                            var"#282#v" = var"#279#cws"[var"#280#wordIndex" + 2] | 0x0000000000000002
                            var"#282#v" |= 0x0000000000000010
                            var"#282#v" |= 0x0000000000000080
                            var"#282#v" |= 0x0000000000000400
                            var"#282#v" |= 0x0000000000002000
                            var"#282#v" |= 0x0000000000010000
                            var"#282#v" |= 0x0000000000080000
                            var"#282#v" |= 0x0000000000400000
                            var"#282#v" |= 0x0000000002000000
                            var"#282#v" |= 0x0000000010000000
                            var"#282#v" |= 0x0000000080000000
                            var"#282#v" |= 0x0000000400000000
                            var"#282#v" |= 0x0000002000000000
                            var"#282#v" |= 0x0000010000000000
                            var"#282#v" |= 0x0000080000000000
                            var"#282#v" |= 0x0000400000000000
                            var"#282#v" |= 0x0002000000000000
                            var"#282#v" |= 0x0010000000000000
                            var"#282#v" |= 0x0080000000000000
                            var"#282#v" |= 0x0400000000000000
                            var"#279#cws"[var"#280#wordIndex" + 2] |= var"#282#v" | 0x2000000000000000
                        end
                        #= /home/Gordon/Documents/JuliaWork/BenchSoE/src/DragRace.jl:124 =#
                        var"#280#wordIndex" += bp
                    end
            $(Expr(:inbounds, :pop))
            var"#285#val"
        end
```
where one can see the marking proceeding as described by 64-bit words for three words in the pattern for a base prime value of three.

For a base prime value of thirteen, the generated code is as follows ("macros.jl" lines 3601 to 3677):

```
        #= /home/Gordon/Documents/JuliaWork/BenchSoE/src/DragRace.jl:121 =#
        begin
            $(Expr(:inbounds, true))
            local var"#290#val" = while var"#280#wordIndex" <= var"#281#wordLimit"
                        #= /home/Gordon/Documents/JuliaWork/BenchSoE/src/DragRace.jl:123 =#
                        begin
                            var"#282#v" = var"#279#cws"[var"#280#wordIndex" + 0] | 0x0000000000000001
                            var"#282#v" |= 0x0000000000002000
                            var"#282#v" |= 0x0000000004000000
                            var"#282#v" |= 0x0000008000000000
                            var"#279#cws"[var"#280#wordIndex" + 0] |= var"#282#v" | 0x0010000000000000
                            var"#282#v" = var"#279#cws"[var"#280#wordIndex" + 1] | 0x0000000000000002
                            var"#282#v" |= 0x0000000000004000
                            var"#282#v" |= 0x0000000008000000
                            var"#282#v" |= 0x0000010000000000
                            var"#279#cws"[var"#280#wordIndex" + 1] |= var"#282#v" | 0x0020000000000000
                            var"#282#v" = var"#279#cws"[var"#280#wordIndex" + 2] | 0x0000000000000004
                            var"#282#v" |= 0x0000000000008000
                            var"#282#v" |= 0x0000000010000000
                            var"#282#v" |= 0x0000020000000000
                            var"#279#cws"[var"#280#wordIndex" + 2] |= var"#282#v" | 0x0040000000000000
                            var"#282#v" = var"#279#cws"[var"#280#wordIndex" + 3] | 0x0000000000000008
                            var"#282#v" |= 0x0000000000010000
                            var"#282#v" |= 0x0000000020000000
                            var"#282#v" |= 0x0000040000000000
                            var"#279#cws"[var"#280#wordIndex" + 3] |= var"#282#v" | 0x0080000000000000
                            var"#282#v" = var"#279#cws"[var"#280#wordIndex" + 4] | 0x0000000000000010
                            var"#282#v" |= 0x0000000000020000
                            var"#282#v" |= 0x0000000040000000
                            var"#282#v" |= 0x0000080000000000
                            var"#279#cws"[var"#280#wordIndex" + 4] |= var"#282#v" | 0x0100000000000000
                            var"#282#v" = var"#279#cws"[var"#280#wordIndex" + 5] | 0x0000000000000020
                            var"#282#v" |= 0x0000000000040000
                            var"#282#v" |= 0x0000000080000000
                            var"#282#v" |= 0x0000100000000000
                            var"#279#cws"[var"#280#wordIndex" + 5] |= var"#282#v" | 0x0200000000000000
                            var"#282#v" = var"#279#cws"[var"#280#wordIndex" + 6] | 0x0000000000000040
                            var"#282#v" |= 0x0000000000080000
                            var"#282#v" |= 0x0000000100000000
                            var"#282#v" |= 0x0000200000000000
                            var"#279#cws"[var"#280#wordIndex" + 6] |= var"#282#v" | 0x0400000000000000
                            var"#282#v" = var"#279#cws"[var"#280#wordIndex" + 7] | 0x0000000000000080
                            var"#282#v" |= 0x0000000000100000
                            var"#282#v" |= 0x0000000200000000
                            var"#282#v" |= 0x0000400000000000
                            var"#279#cws"[var"#280#wordIndex" + 7] |= var"#282#v" | 0x0800000000000000
                            var"#282#v" = var"#279#cws"[var"#280#wordIndex" + 8] | 0x0000000000000100
                            var"#282#v" |= 0x0000000000200000
                            var"#282#v" |= 0x0000000400000000
                            var"#282#v" |= 0x0000800000000000
                            var"#279#cws"[var"#280#wordIndex" + 8] |= var"#282#v" | 0x1000000000000000
                            var"#282#v" = var"#279#cws"[var"#280#wordIndex" + 9] | 0x0000000000000200
                            var"#282#v" |= 0x0000000000400000
                            var"#282#v" |= 0x0000000800000000
                            var"#282#v" |= 0x0001000000000000
                            var"#279#cws"[var"#280#wordIndex" + 9] |= var"#282#v" | 0x2000000000000000
                            var"#282#v" = var"#279#cws"[var"#280#wordIndex" + 10] | 0x0000000000000400
                            var"#282#v" |= 0x0000000000800000
                            var"#282#v" |= 0x0000001000000000
                            var"#282#v" |= 0x0002000000000000
                            var"#279#cws"[var"#280#wordIndex" + 10] |= var"#282#v" | 0x4000000000000000
                            var"#282#v" = var"#279#cws"[var"#280#wordIndex" + 11] | 0x0000000000000800
                            var"#282#v" |= 0x0000000001000000
                            var"#282#v" |= 0x0000002000000000
                            var"#282#v" |= 0x0004000000000000
                            var"#279#cws"[var"#280#wordIndex" + 11] |= var"#282#v" | 0x8000000000000000
                            var"#282#v" = var"#279#cws"[var"#280#wordIndex" + 12] | 0x0000000000001000
                            var"#282#v" |= 0x0000000002000000
                            var"#282#v" |= 0x0000004000000000
                            var"#279#cws"[var"#280#wordIndex" + 12] |= var"#282#v" | 0x0008000000000000
                        end
                        #= /home/Gordon/Documents/JuliaWork/BenchSoE/src/DragRace.jl:124 =#
                        var"#280#wordIndex" += bp
                    end
            $(Expr(:inbounds, :pop))
            var"#290#val"
        end
```

where one can clearly see the marking done as described across the thirteen 64-bit words for a base prime value of thirteen (case five)

### Author

W. Gordon Goodsman (aka GordonBGood)
