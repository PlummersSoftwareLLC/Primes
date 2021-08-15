FROM hseeberger/scala-sbt:11.0.12_1.5.5_3.0.1
WORKDIR /opt/app
COPY project/build.properties project/build.properties
COPY src/main/scala/main.scala src/main/scala/main.scala
COPY build.sbt build.sbt

ENTRYPOINT [ "sbt", "run" ]
