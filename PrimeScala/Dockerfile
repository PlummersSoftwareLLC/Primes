FROM adoptopenjdk:11-jdk-hotspot

WORKDIR /opt/app

COPY sieve/src/Sieve.scala .
COPY build.sc .
COPY mill .
RUN ./mill sieveJVM.run
RUN ./mill sieve.run

ENTRYPOINT [ "./mill", "sieveJVM.run" ]
