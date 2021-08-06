FROM alpine:3.13
RUN apk add --update build-base cmake git
# The last release is 12 years old and doesn't work with current compilers
# so we're using git
ARG CFUNGE_GIT=https://github.com/VorpalBlade/cfunge.git
ARG CFUNGE_COMMIT=e73ed79610bfd3dbdddb84bd599e86559a129fbc
WORKDIR /tmp
RUN git clone -n ${CFUNGE_GIT} cfunge && \
    cd cfunge && git checkout ${CFUNGE_COMMIT} && \
    mkdir build && \
    cd build && \
    cmake .. -DUSE_NCURSES=OFF && \
    make -j8 && make install

WORKDIR /opt/primebefunge
COPY primes.b98 ./
ENTRYPOINT [ "cfunge", "primes.b98" ]