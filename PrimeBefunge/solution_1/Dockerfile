FROM alpine:3.13

RUN apk add --no-cache build-base=0.5-r2 cmake=3.18.4-r1 git=2.30.2-r0
# The latest release (0.9.0) does not work for whatever reason, but the current
# git HEAD, mercifully, does.
ARG CFUNGE_GIT=https://github.com/VorpalBlade/cfunge.git
ARG CFUNGE_COMMIT=e73ed79610bfd3dbdddb84bd599e86559a129fbc
ARG CFUNGE_DIRECTORY=cfunge

WORKDIR /tmp
RUN git clone -n ${CFUNGE_GIT} ${CFUNGE_DIRECTORY}
WORKDIR /tmp/${CFUNGE_DIRECTORY}
RUN git checkout ${CFUNGE_COMMIT} 
WORKDIR /tmp/${CFUNGE_DIRECTORY}/build
RUN cmake .. -DUSE_NCURSES=OFF -DUSE_64BIT=OFF && \
    make && make install

WORKDIR /opt/primebefunge
COPY primes.b98 ./
ENTRYPOINT [ "cfunge", "primes.b98" ]
