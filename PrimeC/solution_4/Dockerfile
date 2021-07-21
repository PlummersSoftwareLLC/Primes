FROM ubuntu

# Get GCC
RUN apt-get update
RUN apt-get install -y build-essential

WORKDIR /home/primes

COPY sieve_memcopy.c run.sh ./
RUN chmod a+x run.sh

ENTRYPOINT ["./run.sh"]
