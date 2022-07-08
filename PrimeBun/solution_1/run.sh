#!/bin/sh
bun run PrimeBun.js

# yet to be implemented as (best I can tell) Bun
# doesn't yet have cluster implemented
# bun run PrimeBun_cluster.js

bun run PrimeBun_memcopy.js
