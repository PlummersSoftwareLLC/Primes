FROM alpine:3.13

RUN apk add --no-cache tar=1.34-r0 libaio=0.3.112-r1 libnsl=1.3.0-r0 libc6-compat=1.2.2-r1 autoconf=2.69-r3 make=4.3-r0 g++=10.2.1_pre1-r3 gmp-dev=6.2.1-r0 db-dev=5.3.28-r1 libxml2-dev=2.9.12-r0

ARG COBOL_BASENAME=gnucobol-3.1-rc1

WORKDIR /opt

# get and install gnucobol
RUN wget --progress=dot:giga https://sourceforge.net/projects/gnucobol/files/gnucobol/3.1/"${COBOL_BASENAME}".tar.gz/download -O "${COBOL_BASENAME}".tar.gz && \
    tar xvfz "${COBOL_BASENAME}".tar.gz 
    
WORKDIR /opt/"${COBOL_BASENAME}"

RUN ./configure && make && make install

WORKDIR /opt

RUN rm -rf "${COBOL_BASENAME}"

WORKDIR /opt/app

COPY ./primes.cbl .
RUN cobc -x primes.cbl

ENTRYPOINT ["./primes"]