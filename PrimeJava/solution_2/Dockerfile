FROM eclipse-temurin:25-jdk-alpine-3.24

WORKDIR /opt/app

COPY src/*.java run.sh ./

ENTRYPOINT [ "sh", "./run.sh" ]