FROM clojure:openjdk-18-tools-deps-1.10.3.1040
WORKDIR /primes
COPY deps.edn sieve.clj run.sh ./
ENTRYPOINT ["./run.sh"]
