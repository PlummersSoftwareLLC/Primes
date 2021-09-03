FROM adoptopenjdk:16-jdk-hotspot

WORKDIR /opt/app

COPY src src
COPY runSolution.sh .
RUN javac src/*.java

CMD [ "sh", "runSolution.sh" ]
