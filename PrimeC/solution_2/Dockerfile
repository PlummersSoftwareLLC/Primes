FROM ubuntu:20.04

# Get GCC
RUN apt-get update
RUN apt-get install -y build-essential

WORKDIR /home/primes

COPY compile.sh .
COPY run.sh .
COPY *.c ./

RUN chmod +x compile.sh run.sh

RUN ./compile.sh

ENTRYPOINT [ "./run.sh" ]


