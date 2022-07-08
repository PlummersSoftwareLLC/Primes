# use the image below as the preffered base image
# see CONTRIBUTING.md
FROM jarredsumner/bun:edge

WORKDIR /opt/app

RUN apt-get update
RUN apt-get install -y curl
RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash -
RUN apt-get install -y nodejs
RUN apt-get autoclean

COPY PrimeBun.ts PrimeBun_memcopy.ts bun.lockb package.json tsconfig.json run.sh ./
RUN bun install
RUN bun run tsc ./PrimeBun.ts ./PrimeBun_memcopy.ts
RUN rm bun.lockb package.json tsconfig.json && rm -r node_modules

ENTRYPOINT [ "./run.sh" ]
