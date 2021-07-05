ARG GRADLE_VERSION=6.8
ARG JDK_VERSION=11

FROM gradle:${GRADLE_VERSION}-jdk-hotspot as BUILD

COPY . /src
WORKDIR /src
RUN gradle shadowJar

FROM adoptopenjdk:${JDK_VERSION}-jdk-hotspot

COPY --from=BUILD /src/build/libs/prime-sieve-1.0-SNAPSHOT-all.jar /bin/runner/run.jar
WORKDIR /bin/runner

ENTRYPOINT ["java", "-jar", "run.jar"]