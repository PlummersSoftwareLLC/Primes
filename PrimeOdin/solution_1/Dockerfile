FROM primeimages/odin:commit-481fc8a5b60cf15d AS build

COPY *.odin /opt/app/
WORKDIR /opt/app
RUN odin build main.odin -opt:3 -no-bounds-check -out:primes-odin-1

FROM primeimages/odin:commit-481fc8a5b60cf15d AS runtime
COPY --from=build /opt/app/primes-odin-1 primes-odin-1

ENTRYPOINT [ "./primes-odin-1" ]
