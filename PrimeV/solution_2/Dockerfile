FROM alpine:3.24 AS build

RUN apk add --no-cache build-base git

# Build V from source: the C bootstrap (vc) commit that matches the V release
ARG V_VERSION=0.5.2
ARG VC_COMMIT=7eb8c54a3843e5107d5af06d7a8c3e928f322475

WORKDIR /opt/v
RUN git clone --depth 1 --branch "${V_VERSION}" https://github.com/vlang/v . \
    && git clone --filter=blob:none https://github.com/vlang/vc vc \
    && git -C vc checkout "${VC_COMMIT}" \
    && cc -std=c99 -w -o v1 vc/v.c -lm -lpthread \
    && ./v1 -no-parallel -cc cc -gc none -o v2 cmd/v \
    && ./v2 -nocache -cc cc -gc none -o v cmd/v \
    && rm -rf v1 v2 vc \
    && ln -s /opt/v/v /usr/local/bin/v

WORKDIR /opt/app
COPY *.v .

RUN v -prod primes.v

FROM alpine:3.24

COPY --from=build /opt/app/primes /usr/local/bin/primes

ENTRYPOINT [ "primes" ]
