FROM eclipse-temurin:25-jdk-noble

RUN apt-get update \
    && apt-get install -y --no-install-recommends curl \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app

COPY sieve/src/Sieve.scala sieve/src/Sieve.scala
COPY build.mill mill ./
RUN ./mill sieveJVM.run
#RUN ./mill sieve.run #this requires llvm

ENTRYPOINT [ "./mill", "sieveJVM.run" ]
