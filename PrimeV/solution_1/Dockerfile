FROM alpine:3.16 AS build

ENV V_VER="0.3"

RUN apk update && apk add --no-cache build-base bash git

WORKDIR /opt/vlang
RUN git clone https://github.com/vlang/v /opt/vlang && git checkout "${V_VER}" && make

ENV PATH=/opt/vlang:$PATH

WORKDIR /opt/app
COPY *.v .

RUN v -prod primes.v

FROM alpine:3.16

COPY --from=build /opt/app/primes /usr/local/bin/primes

ENTRYPOINT [ "primes" ]
