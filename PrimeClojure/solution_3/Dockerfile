FROM clojure:openjdk-18-tools-deps-bullseye

WORKDIR /primes

RUN apt-get update -y && apt-get install -y numactl

COPY deps.edn sieve.clj run.sh ./

ENTRYPOINT ["./run.sh"]
