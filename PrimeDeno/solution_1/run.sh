#!/bin/sh

# Cache the TS file
deno cache PrimeDeno.ts

# Prepare binaries
IS_COMPILED=1 deno compile --allow-env=IS_COMPILED --output PrimeDenoJS.bin PrimeDeno.js
IS_COMPILED=1 deno compile --allow-env=IS_COMPILED --output PrimeDenoTS.bin PrimeDeno.ts

# Run the scripts
IS_COMPILED=0 deno run --allow-env=IS_COMPILED PrimeDeno.js
IS_COMPILED=0 deno run --allow-env=IS_COMPILED PrimeDeno.ts

# Run compiled binaries
./PrimeDenoJS.bin
./PrimeDenoTS.bin
