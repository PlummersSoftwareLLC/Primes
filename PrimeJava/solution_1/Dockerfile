FROM eclipse-temurin:25-jdk-alpine-3.24

RUN apk add --no-cache bash

WORKDIR /opt/app

COPY PrimeSieveJava.java run.sh ./

RUN javac PrimeSieveJava.java

ENTRYPOINT [ "./run.sh" ]