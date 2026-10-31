FROM eclipse-temurin:25-jdk-noble

WORKDIR /opt/app

COPY src src
COPY runSolution.sh .
RUN javac src/*.java

CMD [ "sh", "runSolution.sh" ]
