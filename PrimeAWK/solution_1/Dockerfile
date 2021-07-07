FROM ubuntu

RUN apt-get update
RUN apt-get install -y build-essential
RUN apt-get install mawk


WORKDIR /home/primes

COPY primes.awk counts.csv ./


ENTRYPOINT ["mawk",  "-f", "primes.awk", "-F", ",", "counts.csv"]
