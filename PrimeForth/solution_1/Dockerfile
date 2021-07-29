FROM mtrute/gforth-container:0.7.9_20190627

WORKDIR /opt/app

COPY prime-bitarray.fs prime-bytearray.fs run.sh ./

ENTRYPOINT [ "sh", "run.sh" ]
