FROM ubuntu:24.04

WORKDIR /tmp
RUN apt-get update && \
    apt-get install -y ghc git make python3 && \
    git clone --depth 1 https://github.com/TryItOnline/WSpace && \
    make -C WSpace && \
    cp WSpace/wspace /usr/bin/whitespace && \
    apt-get remove -y git make && \
    apt-get autoremove -y && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/WSpace

WORKDIR /opt/app
COPY *.ws *.sh *.py ./

ENTRYPOINT ["./run_primes.sh"]
