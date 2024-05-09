# We're sticking to VICE 3.5 because in later versions the sequential output file doesn't get
# closed when the "CLOSE" ROM routine is called, making it impossible for c1451 to read it.
FROM primeimages/vice:3.5-ubuntu22.04

WORKDIR /opt

RUN apt-get update && \
    apt-get install -y aspnetcore-runtime-6.0 gawk grep unzip bash socat && \
    rm -rf /var/lib/apt/lists/* && \
    wget https://enginedesigns.net/download/retroassembler.zip && \
    mkdir retroassembler && \
    unzip retroassembler.zip -d retroassembler && \
    rm retroassembler.zip

COPY build.sh dockerrun.sh parse.sh primes.s ./

RUN ./build.sh

ENTRYPOINT ["./dockerrun.sh"]
