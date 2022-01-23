FROM openjdk:17

WORKDIR /opt/app

COPY Sieve.java .
RUN javac Sieve.java

ENTRYPOINT [ "java", "Sieve", "-warmup" ]