FROM fvbakel/kotlin-native:1.5.20 as build

WORKDIR /opt/app
COPY primes-native.kt .
RUN kotlinc-native primes-native.kt -opt -o primes

# Run stage
FROM ubuntu:18.04

WORKDIR /opt/app
COPY --from=build /opt/app/primes.kexe .

ENTRYPOINT ["./primes.kexe"]
