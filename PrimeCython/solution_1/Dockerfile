FROM python:3.9

WORKDIR /root

RUN [ "pip", "install", "cython"]

COPY PrimeCY_bytearray.pyx PrimeCY_bitarray.pyx PrimeCY_32.pyx buildall.sh ./

RUN [ "./buildall.sh" ]

ENTRYPOINT ["sh", "-c", "./PrimeCY_bytearray && ./PrimeCY_bitarray && ./PrimeCY_32"]
