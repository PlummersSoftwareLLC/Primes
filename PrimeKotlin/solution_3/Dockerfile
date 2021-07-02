FROM adoptopenjdk:11-jdk-hotspot as build

ENV KOTLIN_VERSION=1.5.20

#installs
RUN apt-get update && apt-get install wget unzip

RUN cd /opt \
    && wget https://github.com/JetBrains/kotlin/releases/download/v${KOTLIN_VERSION}/kotlin-compiler-${KOTLIN_VERSION}.zip \
    && unzip kotlin-compiler-${KOTLIN_VERSION}.zip

ENV PATH $PATH:/opt/kotlinc/bin

# Build stage
WORKDIR /opt/app
COPY *.kt ./
RUN unzip -j /opt/kotlinc/lib/kotlin-stdlib-js.jar "kotlin.js" -d /opt/app

RUN kotlinc-js -module-kind commonjs primes-js.kt -output primes.js

# Run stage
FROM node:current-alpine3.13

WORKDIR /opt/app

RUN mkdir ./node_modules/
COPY --from=build /opt/app/kotlin.js ./node_modules/
COPY --from=build /opt/app/primes.js ./

ENTRYPOINT [ "node", "primes.js" ]
