FROM python:3.9

WORKDIR /root

RUN [ "pip", "install", "cython"]

COPY PrimeCY.pyx PrimeCY_bitarray.pyx PrimeCY_pointers.pyx buildall.sh ./

RUN [ "./buildall.sh" ]

ENTRYPOINT ./PrimeCY && ./PrimeCY_pointers && ./PrimeCY_bitarray
