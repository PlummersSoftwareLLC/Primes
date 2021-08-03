FROM ghcr.io/graalvm/graalvm-ce:21.2.0

WORKDIR /opt/app

RUN gu install native-image

COPY PrimeSieveJava.java .
RUN javac PrimeSieveJava.java && native-image PrimeSieveJava

ENTRYPOINT [ "./primesievejava" ]