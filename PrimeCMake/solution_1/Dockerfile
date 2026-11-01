FROM alpine:3.24

RUN apk add cmake --no-cache

WORKDIR /home/primes
COPY ./primes.cmake .

ENTRYPOINT ["cmake", "-P", "primes.cmake"]
