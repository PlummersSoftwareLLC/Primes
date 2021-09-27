ARG GRADLE_VERSION=7.1.1
ARG JDK_VERSION=16

FROM gradle:${GRADLE_VERSION}-jdk-hotspot as BUILD

RUN apt-get update && apt-get -y install libncurses5

WORKDIR /src
COPY . .
RUN ./compile.sh

FROM adoptopenjdk:${JDK_VERSION}-jdk-hotspot

WORKDIR /opt/app
#
# Add node to the JDK to allow to run all solutions for different targets in one
# go
ENV NODE_VERSION 16.5.0

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
COPY --from=BUILD /src/build/libs/PrimeSieve-1.0-all.jar /src/build/compileSync/main/productionExecutable/kotlin/PrimeSieve.js /src/build/bin/native/releaseExecutable/PrimeSieve.kexe ./

ENTRYPOINT ["./run.sh"]