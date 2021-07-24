FROM gradle:6-jdk11

WORKDIR /opt/app

COPY . .

RUN gradle jar

COPY . .

CMD [ "java", "-jar", "build/libs/primes-1.0-SNAPSHOT.jar" ]
