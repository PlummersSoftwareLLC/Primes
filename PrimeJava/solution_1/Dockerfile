FROM openjdk:8

WORKDIR /opt/app

COPY PrimeSieveJava.java .
RUN javac PrimeSieveJava.java

ENTRYPOINT [ "java", "PrimeSieveJava", "-warmup" ]