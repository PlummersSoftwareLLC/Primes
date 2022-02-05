FROM openjdk:18

WORKDIR /opt/app

COPY PrimeSieveJava.java run.sh ./
RUN javac PrimeSieveJava.java

ENTRYPOINT [ "./run.sh" ]