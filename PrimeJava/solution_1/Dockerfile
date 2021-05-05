FROM adoptopenjdk:11-jdk-hotspot

WORKDIR /opt/app

COPY src/main/java/PrimeSieveJava.java .
RUN javac PrimeSieveJava.java

ENTRYPOINT [ "java", "PrimeSieveJava" ]