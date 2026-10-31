FROM eclipse-temurin:25-jdk-noble

WORKDIR /opt/app
COPY PrimeYoix.yx .

ADD https://github.com/att/yoix/raw/1d83e36c004367d910357bc19e6cc064dc3457e1/yoix.jar yoix.jar

ENTRYPOINT [ "java", "-jar", "yoix.jar", "PrimeYoix.yx" ]
