# Deno solution by hktr92
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-red)
![Bit count](https://img.shields.io/badge/Bits-1-green)

Implementation in Deno, with a 32 bit integer array as buffer for the bit array.
This implementation is based on the logic from:
- NodeJS implementation   of NodeJS/solution_1 by Rogier van Dam
- previous implementation of NodeJS/solution_1 by Frank van Bakel
- Python/solution_2                            by ssovest
- PrimeCPP                                     by Dave Plummer

**PrimeDeno.js** - The base algorithm follows the original one by Dave Plummer, the only exception being that the factors themselves are divided by 2 at the start.

**PrimeDeno.ts** - Same as JS file, but with typing

**PrimeDenoJS.bin** - JS variant, but compiled into a binary file

**PrimeDenoTS.bin** - TS variant, but compiled into a binary file

## Variants breakdown
There are two main categories:

1. Scripted:
   - The JS variant is almost as-is from NodeJS/solution_1 (without any of NodeJS' dependency management)
   - The TS variant is derived from NodeJS/solution_1 JS file and has nothing to do with TypeScript/solution_*
     - It's like this because Deno can run both of them, the only extra layer is the TS -> JS conversion done by [swc compiler](https://github.com/swc-project/swc).
2. Compiled
   - When running `compile` command, it actually happens these:
     - the input code is `bundle`d
     - the output is appended at the end of `deno` binary
     - `deno` binary is executing automatically the embedded javascript code
       - for more details: [Compiling executables on Deno Manual](https://deno.land/manual/tools/compiler)

In order to distinct between scripted and compiled, the `--allow-env=IS_SCRIPTED` permission is granted.

## Differences between Deno and Node
- Deno is secure by default: you need to grant permissions for various system tasks (see: [Permissions on Deno Manual](https://deno.land/manual/getting_started/permissions)).
  - e.g. for starting a web server, you have to run `deno run --allow-net server.ts`.
- Deno is written in Rust
- Deno supports both JavaScript and TypeScript out of the box.
- Deno has a decentralized, URL-based import system. There's a single entrypoint for deps: `deps.ts` (or `.js`)
- Deno has various built-in tools:
  - Formatter (NodeJS alternative(s): eslint with `--fix` CLI flag)
  - Linter (NodeJS alternative(s): eslint)
  - Bundler (NodeJS alternative(s): webpack, parcel, esbuild, ...)
  - Compiler (NodeJS alternative(s): [vercel/pkg](https://github.com/vercel/pkg))
  - Test framework (NodeJS alternative(s): mocha, chai, ...)

## Run instructions
Install Deno: <https://deno.land/>

```bash
cd path/to/sieve
deno run PrimeDeno.js
deno run PrimeDeno.ts

# Compiling the binary
deno compile PrimeDeno.js PrimeDenoJS.bin && ./PrimeDenoJS
deno compile PrimeDeno.ts PrimeDenoTS.bin && ./PrimeDenoTS
```

## Output
Below is an example of the output

    hktr92-deno-js-script;5724;5;1;algorithm=base,faithful=yes,bits=1
    hktr92-deno-ts-script;5827;5;1;algorithm=base,faithful=yes,bits=1
    hktr92-deno-js-compiled;5880;5;1;algorithm=base,faithful=yes,bits=1
    hktr92-deno-ts-compiled;5839;5;1;algorithm=base,faithful=yes,bits=1
