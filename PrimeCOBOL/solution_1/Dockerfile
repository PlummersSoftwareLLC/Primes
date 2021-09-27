FROM alpine:3.13

RUN apk add --no-cache tar libaio libnsl libc6-compat autoconf make g++ gmp-dev db-dev libxml2-dev

ARG COBOL_BASENAME=gnucobol-3.1-rc1

WORKDIR /opt

# get and install gnucobol
RUN wget https://sourceforge.net/projects/gnucobol/files/gnucobol/3.1/"${COBOL_BASENAME}".tar.gz/download -O "${COBOL_BASENAME}".tar.gz && \
    tar xvfz "${COBOL_BASENAME}".tar.gz 
    
WORKDIR /opt/"${COBOL_BASENAME}"

RUN ./configure && make && make install

WORKDIR /opt

RUN rm -rf "${COBOL_BASENAME}"

WORKDIR /opt/app

COPY ./primes.cbl .
RUN cobc -x primes.cbl

ENTRYPOINT ["./primes"]
