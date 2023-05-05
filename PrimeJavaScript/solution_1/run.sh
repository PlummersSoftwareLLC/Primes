#!/bin/sh

node PrimeJavaScript.js
bun PrimeJavaScript.js
deno run PrimeJavaScript.js

node PrimeJavaScript_memcopy.js
deno run PrimeJavaScript_memcopy.js
bun PrimeJavaScript_memcopy.js

node PrimeJavaScript_cluster.js base
node PrimeJavaScript_cluster.js memcopy

# cluster not currently supported by Bun
# node PrimeJavaScript_cluster.js

node PrimeJavaScript_worker_main.mjs

# worker_main in deno not working because of failed imports
# deno run PrimeJavaScript_worker_main.mjs

# worker_threads not currently supported by Bun
# bun PrimeJavaScript_worker_main.mjs

