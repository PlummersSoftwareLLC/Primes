FROM alpine:3.13

WORKDIR /opt/app
RUN apk add tar libaio libnsl libc6-compat autoconf make g++ gmp-dev db-dev libxml2-dev

# get and install gnucobol
RUN wget https://sourceforge.net/projects/gnucobol/files/gnucobol/3.1/gnucobol-3.1-rc1.tar.gz/download -O gnucobol-3.1-rc1.tar.gz && \
    tar xvfz gnucobol-3.1-rc1.tar.gz && cd gnucobol-3.1-rc1 && ./configure && make && make install && cd ..

COPY ./primes.cbl .
RUN cobc -x primes.cbl

ENTRYPOINT ["./primes"]