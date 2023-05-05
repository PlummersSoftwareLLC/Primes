FROM thevlang/vlang:alpine-dev AS build

WORKDIR /opt/app
COPY *.v .

RUN v -prod primes.v

FROM thevlang/vlang:alpine

COPY --from=build /opt/app/primes /usr/local/bin/primes

ENTRYPOINT [ "primes" ]
