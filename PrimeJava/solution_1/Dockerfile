FROM openjdk:8-bullseye

WORKDIR /opt/app

RUN apt-get update -y && apt-get install -y numactl

COPY PrimeSieveJava.java run.sh ./

RUN javac PrimeSieveJava.java

ENTRYPOINT [ "./run.sh" ]