FROM ghcr.io/graalvm/native-image-community:25i4-25.0.4.1.1-ol10

WORKDIR /opt/app

COPY PrimeSieveJava.java .
RUN javac PrimeSieveJava.java && native-image PrimeSieveJava

ENTRYPOINT [ "./primesievejava" ]
