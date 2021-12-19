FROM openjdk:17-jdk-slim-buster

RUN apt-get update && \
    apt-get install -y gawk grep build-essential wget && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY eula.txt server.properties buildioscript.txt runioscript.txt playio.c run.sh ./

RUN wget https://launcher.mojang.com/v1/objects/125e5adf40c659fd3bce3e66e67a16bb49ecc1b9/server.jar && \
    gcc playio.c -o playio && \
    ./playio java -Xmx1024M -Xms1024M -jar server.jar nogui < buildioscript.txt && \
    mkdir world/datapacks/Pack

COPY Pack world/datapacks/Pack/

ENTRYPOINT [ "./run.sh" ]