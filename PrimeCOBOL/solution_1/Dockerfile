FROM alpine:3.13

RUN apk add tar libaio libnsl libc6-compat autoconf make g++ gmp-dev db-dev libxml2-dev

WORKDIR /opt

# get and install gnucobol
RUN wget https://sourceforge.net/projects/gnucobol/files/gnucobol/3.1/gnucobol-3.1-rc1.tar.gz/download -O gnucobol-3.1-rc1.tar.gz && \
    tar xvfz gnucobol-3.1-rc1.tar.gz 
    
WORKDIR /opt/gnucobol-3.1-rc1

RUN ./configure && make && make install

WORKDIR /opt

RUN rm -rf gnucobol-3.1-rc1

WORKDIR /opt/app

COPY ./primes.cbl .
RUN cobc -x primes.cbl

ENTRYPOINT ["./primes"]