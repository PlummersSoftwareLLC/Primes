ARG GRADLE_VERSION=8.0.1
ARG JDK_VERSION=19

FROM gradle:${GRADLE_VERSION}-jdk${JDK_VERSION}-jammy as BUILD

RUN apt-get update && apt-get -y install libncurses5

WORKDIR /src
COPY . .
RUN ./compile.sh
# Ensure that nativeExecutable product exists for Docker build to complete successfully, despite not being used in ARM Linux
RUN ./compile.sh && \
    mkdir -p /src/build/bin/native/releaseExecutable && \
    touch /src/build/bin/native/releaseExecutable/PrimeSieve.kexe

FROM eclipse-temurin:${JDK_VERSION}-jdk-jammy

WORKDIR /opt/app
#
# Add node to the JDK to allow to run all solutions for different targets in one
# go
ENV NODE_VERSION 19.7.0

RUN ARCH= && dpkgArch="$(dpkg --print-architecture)" \
  && case "${dpkgArch##*-}" in \
    amd64) ARCH='x64';; \
    ppc64el) ARCH='ppc64le';; \
    s390x) ARCH='s390x';; \
    arm64) ARCH='arm64';; \
    armhf) ARCH='armv7l';; \
    i386) ARCH='x86';; \
    *) echo "unsupported architecture"; exit 1 ;; \
  esac \
  && mkdir /bin/node \
  && apt-get update && apt-get -y install xz-utils  \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* \
  && curl -fsSLO --compressed "https://nodejs.org/dist/v$NODE_VERSION/node-v$NODE_VERSION-linux-$ARCH.tar.xz" \
  && tar -xJf "node-v$NODE_VERSION-linux-$ARCH.tar.xz" -C /bin/node --strip-components=1

COPY ./run.sh ./
COPY --from=BUILD /src/build/libs/PrimeSieve-1.0-all.jar /src/build/compileSync/js/main/productionExecutable/kotlin/*.js /src/build/bin/native/releaseExecutable/PrimeSieve.kexe ./

ENTRYPOINT ["./run.sh"]
