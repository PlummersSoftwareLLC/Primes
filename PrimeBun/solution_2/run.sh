#!/bin/sh

# prevents Bun from doing it's own optimisations of the JS files
# as we want to directly test tsc's output
BUN_DISABLE_TRANSPILER=1 bun run PrimeBun.js
BUN_DISABLE_TRANSPILER=1 bun run PrimeBun_memcopy.js

bun run PrimeBun.ts
bun run PrimeBun_memcopy.ts

# yet to be implemented as (best I can tell) Bun
# doesn't yet have cluster implemented
# bun run PrimeBun_cluster.js
# bun run PrimeBun_cluster.ts

