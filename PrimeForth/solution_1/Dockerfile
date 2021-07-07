FROM mtrute/gforth-container:latest

WORKDIR /opt/app

COPY prime-bitarray.fs prime-bytearray.fs run.sh ./

ENTRYPOINT [ "sh", "run.sh" ]
