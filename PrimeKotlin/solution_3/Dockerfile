ARG GRADLE_VERSION=6.8
ARG JDK_VERSION=16

FROM gradle:${GRADLE_VERSION}-jdk-hotspot as BUILD

COPY . /src
WORKDIR /src
RUN gradle shadowJar
RUN gradle jsNodeProductionRun
# RUN gradle linkReleaseExecutableNative

FROM adoptopenjdk:${JDK_VERSION}-jdk-hotspot

COPY --from=BUILD /src/build/libs/PrimeSieve-1.0-all.jar /bin/runner/run.jar
WORKDIR /bin/runner

ENTRYPOINT ["java", "-jar", "run.jar", "-t -if -i -t:m -if:m -i:m"]

FROM node:16-alpine3.11

COPY --from=BUILD /src/build/compileSync/main/productionExecutable/kotlin/PrimeSieve.js /bin/runner/run.js
WORKDIR /bin/runner

ENTRYPOINT ["node", "run.js", "-t -if -i"]

# FROM alpine:3.14.0

# COPY --from=BUILD /src/build/bin/native/releaseExecutable/PrimeSieve /bin/runner/run
# WORKDIR /bin/runner
# RUN ["chmod", "+x", "run"]

# ENTRYPOINT ["./run", "-t -if -i"]