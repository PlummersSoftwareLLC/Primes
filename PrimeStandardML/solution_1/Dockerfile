FROM silkeh/clang:12
# download libgmp-dev for mlton
# download/extract mlton release
# configure mlton to use clang
RUN apt-get update -y && \
    apt-get install libgmp-dev -y && \
    wget https://github.com/MLton/mlton/releases/download/on-20210117-release/mlton-20210117-1.amd64-linux-glibc2.31.tgz && \
    tar -zxf mlton-20210117-1.amd64-linux-glibc2.31.tgz && \
    cp -fpR "/mlton-20210117-1.amd64-linux-glibc2.31/bin/mlton" "/mlton-20210117-1.amd64-linux-glibc2.31/bin/mlton.bak" && \
    sed \
        -e "s;^CC=.*;CC=\"clang\";" \
        < "/mlton-20210117-1.amd64-linux-glibc2.31/bin/mlton.bak" > "/mlton-20210117-1.amd64-linux-glibc2.31/bin/mlton" && \
    chmod a+x "/mlton-20210117-1.amd64-linux-glibc2.31/bin/mlton" && \
    rm -rf "/mlton-20210117-1.amd64-linux-glibc2.31/bin/mlton.bak"
WORKDIR /app
COPY sml_primes.sml primes.mlb run.sml ./
RUN /mlton-20210117-1.amd64-linux-glibc2.31/bin/mlton -link-opt '-static' -codegen llvm primes.mlb
CMD ["./primes"]