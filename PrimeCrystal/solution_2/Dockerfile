# The Crystal project's own image. Installing via the openSUSE build service
# repo instead means depending on its mirror network, which is not always in
# sync and intermittently returns 404 for a package that exists upstream.
FROM crystallang/crystal:1.21.0

WORKDIR /app

COPY primes.cr .

RUN crystal build --release --no-debug primes.cr

ENTRYPOINT [ "/app/primes" ]
