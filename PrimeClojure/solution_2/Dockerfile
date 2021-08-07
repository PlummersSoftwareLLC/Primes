FROM clojure:openjdk-17-tools-deps-alpine
WORKDIR /primes
COPY deps.edn primes.clj ./
ENTRYPOINT ["clojure", "-X:run"]
