FROM primeimages/vice:3.5

WORKDIR /opt

RUN wget https://packages.microsoft.com/config/ubuntu/20.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb && \
    dpkg -i packages-microsoft-prod.deb && \
    rm packages-microsoft-prod.deb && \
    apt-get update && \
    apt-get install -y apt-transport-https && \
    apt-get update && \
    apt-get install -y aspnetcore-runtime-6.0 gawk grep unzip bash socat && \
    rm -rf /var/lib/apt/lists/* && \
    wget https://enginedesigns.net/download/retroassembler.zip && \
    mkdir retroassembler && \
    unzip retroassembler.zip -d retroassembler && \
    rm retroassembler.zip

COPY build.sh dockerrun.sh parse.sh primes.s ./

RUN ./build.sh

ENTRYPOINT ["./dockerrun.sh"]
