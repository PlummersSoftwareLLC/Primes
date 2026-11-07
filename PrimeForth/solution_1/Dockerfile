FROM alpine:3.24 AS build

ARG GFORTH_VERSION=0.7.9_20190627
ARG GFORTH_SHA256=49fb6431fffc2309d526e243b1699996e663e4824ea6fbc3f267f7b8da02d0de

RUN apk add --no-cache build-base m4 libtool libffi-dev

WORKDIR /tmp
ADD https://www.complang.tuwien.ac.at/forth/gforth/Snapshots/${GFORTH_VERSION}/gforth-${GFORTH_VERSION}.tar.xz gforth.tar.xz
RUN echo "${GFORTH_SHA256}  gforth.tar.xz" | sha256sum -c - \
    && tar xJf gforth.tar.xz

# This snapshot predates C23 (GCC 15 default) and GCC 14 turning implicit declarations into errors.
# Without -fpermissive, configure checks fail and dynamic native code generation gets disabled.
WORKDIR /tmp/gforth-${GFORTH_VERSION}
RUN ./configure --prefix=/usr/local CC="gcc -std=gnu17 -fpermissive" \
    && make \
    && make install

FROM alpine:3.24

RUN apk add --no-cache libltdl libffi

COPY --from=build /usr/local /usr/local

WORKDIR /opt/app

COPY prime-bitarray.fs prime-bytearray.fs run.sh ./

ENTRYPOINT [ "sh", "run.sh" ]
