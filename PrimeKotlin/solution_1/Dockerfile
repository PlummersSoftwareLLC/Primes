FROM openjdk:16-jdk-slim as builder
COPY . .
RUN chmod +x ./gradlew
RUN ./gradlew installDist

FROM openjdk:16-slim
COPY --from=builder ./build/install/PrimeKotlin-solution_1 /app/
WORKDIR /app/
CMD ["./bin/PrimeKotlin-solution_1"]
