FROM adoptopenjdk/openjdk8:x86_64-alpine-jdk8u402-b06-slim

RUN apk add --no-cache bash

WORKDIR /opt/app

COPY PrimeSieveJava.java run.sh ./

RUN javac PrimeSieveJava.java

ENTRYPOINT [ "./run.sh" ]