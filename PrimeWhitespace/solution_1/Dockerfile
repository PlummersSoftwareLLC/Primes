FROM ubuntu:24.04
RUN apt-get update && \
    apt-get install -y git make python3 ghc && \
    mkdir -p /opt && \
    cd /opt && \
    git clone --depth 1 https://github.com/TryItOnline/WSpace && \
    cd WSpace && \
    make && \
    cp wspace /usr/bin/whitespace && \
    cd / && \
    apt-get remove -y git make && \
    apt-get autoremove -y && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /opt/WSpace

WORKDIR /opt/app
COPY *.ws *.sh *.py ./

ENTRYPOINT ["./run_primes.sh"]
