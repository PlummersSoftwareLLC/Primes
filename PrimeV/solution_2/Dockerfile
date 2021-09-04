FROM alpine:3.13 AS build

RUN apk update && apk add --no-cache build-base bash git

WORKDIR /opt/vlang
RUN git clone https://github.com/vlang/v /opt/vlang && git checkout 0.2.4 && make

ENV PATH=/opt/vlang:$PATH

WORKDIR /opt/app
COPY *.v .

RUN v -prod primes.v

FROM alpine:3.13

COPY --from=build /opt/app/primes /usr/local/bin/primes

ENTRYPOINT [ "primes" ]
