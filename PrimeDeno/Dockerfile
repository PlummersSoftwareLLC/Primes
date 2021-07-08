# https://hub.docker.com/r/denoland/deno
# Based on alpine
FROM denoland/deno

WORKDIR /opt/app

COPY PrimeDeno.ts .
RUN deno cache PrimeDeno.ts

ENTRYPOINT [ "deno", "run", "PrimeDeno.ts" ]