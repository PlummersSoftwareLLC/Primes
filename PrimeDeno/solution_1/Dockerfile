# use the image below as the preffered base image
# see CONTRIBUTING.md
FROM denoland/deno:alpine-1.17.1

WORKDIR /app

COPY PrimeDeno.js PrimeDeno.ts run.sh ./

ENTRYPOINT ["./run.sh"]
