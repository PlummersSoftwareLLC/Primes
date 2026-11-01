ARG GRADLE_VERSION=9.8.2
ARG JDK_VERSION=25
ARG NODE_VERSION=26.13.0

FROM node:${NODE_VERSION}-trixie-slim AS node

FROM gradle:${GRADLE_VERSION}-jdk${JDK_VERSION}-noble AS build

WORKDIR /src
COPY . .
# Kotlin/Native is not built on ARM Linux; create an empty placeholder there so the COPY below succeeds
RUN ./compile.sh && \
    mkdir -p /src/build/bin/native/releaseExecutable && \
    touch -a /src/build/bin/native/releaseExecutable/PrimeSieve.kexe

FROM eclipse-temurin:${JDK_VERSION}-jdk-noble

WORKDIR /opt/app

# Add node to the JDK to allow to run all solutions for different targets in one go
RUN apt-get update \
    && apt-get install -y --no-install-recommends libatomic1 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
COPY --from=node /usr/local/bin/node /usr/local/bin/node

COPY ./run.sh ./
COPY --from=build /src/build/libs/PrimeSieve-1.0-all.jar /src/build/compileSync/js/main/productionExecutable/kotlin/*.js /src/build/bin/native/releaseExecutable/PrimeSieve.kexe ./

ENTRYPOINT ["./run.sh"]
