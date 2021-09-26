FROM python:3.9.5

WORKDIR /root

RUN [ "pip", "install", "--no-cache-dir", "Cython==0.29.24" ]

COPY PrimeCY_bytearray.pyx PrimeCY_bitarray.pyx PrimeCY_32.pyx buildall.sh ./

RUN [ "./buildall.sh" ]

ENTRYPOINT [ "sh", "-c", "./PrimeCY_bytearray && ./PrimeCY_bitarray && ./PrimeCY_32" ]