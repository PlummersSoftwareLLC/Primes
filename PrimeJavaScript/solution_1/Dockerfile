FROM oven/bun:1.4.4-debian AS bun

FROM denoland/deno:bin-2.9.8 AS deno

FROM node:26-trixie-slim

COPY --from=bun /usr/local/bin/bun /usr/local/bin/bun
COPY --from=deno /deno /usr/local/bin/deno

WORKDIR /opt/app

COPY run.sh \
     PrimeJavaScript.js \
     PrimeJavaScript_memcopy.js \
     PrimeJavaScript_cluster.js \
     PrimeJavaScript_worker_main.mjs \
     PrimeJavaScript_worker_child.mjs \
     PrimeSieve.mjs \
    ./

# RUN echo "" && \
#     echo "Node version: $(node --version)" && \
#     echo "Bun version: $(bun --version)" && \
#     echo "Deno version: $(deno --version)" && \
#     echo ""

ENTRYPOINT [ "./run.sh" ]

# For manual debugging
#ENTRYPOINT [ "/bin/bash" ]
