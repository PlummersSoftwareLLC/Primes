FROM clojure:openjdk-18-tools-deps-alpine
WORKDIR /primes
COPY deps.edn sieve_1_bit.clj sieve_8_bit.clj run.sh ./
ENTRYPOINT ["./run.sh"]
