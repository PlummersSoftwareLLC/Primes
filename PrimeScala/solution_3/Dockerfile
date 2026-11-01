FROM sbtscala/scala-sbt:eclipse-temurin-25.0.5_8_1.13.1_3.3.9
WORKDIR /opt/app
COPY project/build.properties project/build.properties
COPY src/main/scala/main.scala src/main/scala/main.scala
COPY build.sbt build.sbt
RUN sbt compile

ENTRYPOINT [ "sbt", "run" ]
