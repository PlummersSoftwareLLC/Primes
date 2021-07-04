FROM mtrute/gforth-container:latest

WORKDIR /opt/app

COPY prime-bitarray.fs .

ENTRYPOINT [ "gforth-fast", "prime-bitarray.fs", "-e", "5 1000000 print-benchmark-results bye" ]
