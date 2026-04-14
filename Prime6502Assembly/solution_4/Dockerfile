# Adapted from the C128 solution Dockerfile
FROM primeimages/vice:3.5-ubuntu22.04

WORKDIR /opt

RUN apt-get update && \
    apt-get install -y python3 default-jre unzip bash && \
    rm -rf /var/lib/apt/lists/* && \
    wget https://theweb.dk/KickAssembler/KickAssembler.zip && \
    mkdir KickAssembler && \
    unzip KickAssembler.zip -d KickAssembler && \
    rm KickAssembler.zip

COPY basevars.sh build.sh run.sh PrimeSieve1M.asm analyze_memdump.py memdump_utils.py ./

RUN ./build.sh

ENTRYPOINT ["./run.sh"]
