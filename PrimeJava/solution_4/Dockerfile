FROM adoptopenjdk/openjdk15:jdk-15.0.2_7-alpine-slim

WORKDIR /opt/app

COPY src src
COPY runSolution.sh .
RUN javac src/*.java

CMD [ "sh", "runSolution.sh" ]
