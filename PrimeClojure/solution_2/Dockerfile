FROM clojure:temurin-19-tools-deps-bullseye-slim
WORKDIR /primes
COPY deps.edn sieve_1_bit.clj sieve_8_bit.clj run.sh ./
ENTRYPOINT ["./run.sh"]
