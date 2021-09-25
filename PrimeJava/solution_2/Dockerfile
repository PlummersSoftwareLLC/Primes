FROM adoptopenjdk/openjdk16:jdk-16.0.1_9-alpine-slim

WORKDIR /opt/app

COPY src/*.java ./
COPY run.sh .
RUN chmod a+x run.sh

ENTRYPOINT [ "sh", "./run.sh" ]
