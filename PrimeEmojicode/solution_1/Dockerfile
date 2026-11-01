FROM ubuntu:24.04 AS build

ARG NCURSES_VERSION=6.5

# hadolint ignore=DL3008,DL3015
RUN apt-get update -qq \
    && apt-get install -y build-essential ca-certificates curl rsync

# The Emojicode 1.0-beta.2 compiler is linked against libtinfo.so.5 (ncurses
# ABI 5), which Ubuntu 24.04 no longer packages. Build that library from the
# ncurses sources, the same way Debian built its libtinfo5 package.
WORKDIR /opt/ncurses
# hadolint ignore=DL4006
RUN curl -fsSL "https://ftp.gnu.org/gnu/ncurses/ncurses-${NCURSES_VERSION}.tar.gz" | tar xz --strip-components 1 \
    && ./configure --with-shared --with-termlib --with-abi-version=5 --with-versioned-syms \
        --without-normal --without-debug --without-cxx-binding --without-ada \
        --without-progs --without-tests --without-manpages \
        --with-default-terminfo-dir=/usr/share/terminfo \
        --with-terminfo-dirs=/etc/terminfo:/lib/terminfo:/usr/share/terminfo \
    && make -j"$(nproc)" libs \
    && cp -P lib/libtinfo.so.5* /usr/local/lib/ \
    && ldconfig

WORKDIR /opt/emojicode

# hadolint ignore=DL4006
RUN curl -sL https://github.com/emojicode/emojicode/releases/download/v1.0-beta.2/Emojicode-1.0-beta.2-Linux-x86_64.tar.gz  | tar zxv --strip-components 1 \
    && yes | ./install.sh

WORKDIR /opt/app
COPY primes.emojic .
RUN emojicodec primes.emojic

FROM ubuntu:24.04

COPY --from=build /opt/app/primes /usr/local/bin

ENTRYPOINT [ "primes" ]
