FROM alpine:3.13 AS build

# hadolint ignore=DL3018
RUN sed -i -e 's/v[[:digit:]]\..*\//edge\//g' /etc/apk/repositories \
    && apk add --no-cache build-base crystal

WORKDIR /opt/app
COPY primes.cr .
RUN crystal build primes.cr --release --static --no-debug

FROM alpine:3.13

COPY --from=build /opt/app/primes /usr/local/bin/

ENTRYPOINT [ "primes" ]
