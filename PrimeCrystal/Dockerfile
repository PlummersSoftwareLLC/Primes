FROM alpine:3.13 AS build

RUN apk add --no-cache crystal build-base

WORKDIR /opt/app
COPY primes.cr .
RUN crystal build --release --static --no-debug --verbose primes.cr

FROM alpine:3.13

COPY --from=build /opt/app/primes /usr/local/bin

CMD [ "primes" ]