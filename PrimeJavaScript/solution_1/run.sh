#!/bin/sh

node PrimeJavaScript.js
bun PrimeJavaScript.js
deno run PrimeJavaScript.js




node PrimeJavaScript_memcopy.js
bun PrimeJavaScript_memcopy.js

# needs adaptation because Deno has its own Buffer API
# deno run PrimeJavaScript_memcopy.js




node PrimeJavaScript_cluster.js

# cluster not currently supported by Bun
# node PrimeJavaScript_cluster.js

# needs adaptation because Deno has its own Buffer API
# and I have no idea whether it supports cluster
# deno run PrimeJavaScript_memcopy.js




node PrimeJavaScript_worker_main.mjs

# worker_threads not currently supported by Bun
# bun PrimeJavaScript_worker_main.mjs

# needs adaptation because Deno has its own Buffer API
# and I have no idea whether it supports worker_threads
# deno run PrimeJavaScript_memcopy.js
