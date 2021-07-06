FROM python:3.9

WORKDIR /root

RUN [ "pip", "install", "cython"]

COPY PrimeCY.pyx .

RUN [ "cython", "-3", "--embed", "PrimeCY.pyx", "-o", "PrimeCY.c" ]
RUN [ "gcc", "-I", "/usr/local/include/python3.9", "-l", "python3.9", "-lm", "PrimeCY.c", "-o", "PrimeCY"]

ENTRYPOINT [ "./PrimeCY" ]
