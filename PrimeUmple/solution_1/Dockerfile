FROM eclipse-temurin:25-jdk-noble

WORKDIR /opt/app
COPY PrimeUmple.ump .

ADD https://github.com/umple/umple/releases/download/v1.38.0/umple-1.38.0.8918.bda708970.jar umple.jar
RUN java -jar umple.jar PrimeUmple.ump

ENTRYPOINT [ "java", "PrimeUmple.java" ]
