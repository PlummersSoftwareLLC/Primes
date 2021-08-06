FROM adoptopenjdk:11-jdk-hotspot

WORKDIR /opt/app

COPY PrimeSieveJava.java .
RUN javac PrimeSieveJava.java

ENTRYPOINT [ "java", "PrimeSieveJava" ]