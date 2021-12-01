FROM openjdk:11

WORKDIR /opt/app
COPY PrimeUmple.ump .

RUN curl -sL https://try.umple.org/scripts/umple.jar -o umple.jar \
    && java -jar umple.jar PrimeUmple.ump

ENTRYPOINT [ "java", "PrimeUmple.java" ]