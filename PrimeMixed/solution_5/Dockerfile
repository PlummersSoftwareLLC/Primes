FROM ubuntu:20.04

# Get GCC
RUN apt-get update && apt-get install -y build-essential \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /home/primes

COPY main.cpp run.sh ./

ENTRYPOINT ["./run.sh"]