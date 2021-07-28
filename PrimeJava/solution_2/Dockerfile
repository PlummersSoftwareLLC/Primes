FROM adoptopenjdk/openjdk11:jdk-11.0.11_9-alpine-slim

WORKDIR /opt/app

COPY src/*.java ./
COPY run.sh .
RUN chmod a+x run.sh

ENTRYPOINT [ "sh", "./run.sh" ]
