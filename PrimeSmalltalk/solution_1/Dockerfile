FROM ubuntu:24.04 AS build

# GNU Smalltalk is no longer packaged after Ubuntu 22.04, so build the same
# release the solution was written for (3.2.5) from the GNU source tarball.
ARG GST_VERSION=3.2.5

RUN apt-get update \
    && apt-get install -y build-essential ca-certificates curl gawk pkg-config xz-utils zip \
        libffi-dev libgmp-dev libltdl-dev libreadline-dev libsigsegv-dev zlib1g-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# -fcommon: the 3.2.5 sources rely on tentative definitions, which GCC 10+
# rejects by default.
WORKDIR /opt/smalltalk
RUN curl -fsSL "https://ftp.gnu.org/gnu/smalltalk/smalltalk-${GST_VERSION}.tar.xz" | tar xJ --strip-components 1 \
    && ./configure --prefix=/usr/local --without-emacs --without-tk \
        --with-system-libffi --with-system-libsigsegv --with-system-libltdl \
        CFLAGS="-O2 -fcommon" \
    && make -j"$(nproc)" \
    && make install

FROM ubuntu:24.04

RUN apt-get update \
    && apt-get install -y libffi8 libgmp10 libltdl7 libreadline8t64 libsigsegv2 zlib1g \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

COPY --from=build /usr/local /usr/local
RUN ldconfig

WORKDIR /opt/app

COPY ./primes.st .

ENTRYPOINT ["gst","-g","./primes.st"]
