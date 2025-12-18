FROM eclipse-temurin:11-jdk

WORKDIR /opt/app
COPY PrimeYoix.yx .

RUN curl -sL https://github.com/att/yoix/raw/master/yoix.jar -o yoix.jar

ENTRYPOINT [ "java", "-jar", "yoix.jar", "PrimeYoix.yx" ]