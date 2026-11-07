FROM clojure:temurin-25-tools-deps-1.12.6.1673-alpine
WORKDIR /primes
COPY deps.edn sieve_1_bit.clj sieve_8_bit.clj run.sh ./
ENTRYPOINT ["./run.sh"]
