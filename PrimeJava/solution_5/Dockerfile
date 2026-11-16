FROM eclipse-temurin:25-jdk-alpine-3.24

WORKDIR /opt/app

COPY *.java run.sh ./

# -Xlint:-removal: Unchecked.java uses sun.misc.Unsafe on purpose.
RUN javac --add-modules jdk.incubator.vector -Xlint:-removal *.java && chmod +x run.sh

ENTRYPOINT [ "./run.sh" ]
