FROM ubuntu:20.04
RUN apt-get update \
	&& apt-get install -y regina-rexx \
 	&& apt-get clean \
 	&& rm -rf /var/lib/apt/lists/*
WORKDIR /home/rexx
COPY PrimeREXX.rex .

# Print the required output
ENTRYPOINT ["rexx", "./PrimeREXX", "1000000", "0"]

# Print the primes <= 1000000
#ENTRYPOINT ["rexx", "./PrimeREXX", "1000000", "1"]

# Print a summary of the prime counts up to 100, 1000, 10000, 100000 and 1000000
#ENTRYPOINT ["rexx", "./PrimeREXX", "1000000", "2"]

# Print the primes <= 1000000 AND also a summary as above
#ENTRYPOINT ["rexx", "./PrimeREXX", "1000000", "3"]