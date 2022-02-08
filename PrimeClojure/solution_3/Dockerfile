FROM clojure:openjdk-18-tools-deps

WORKDIR /primes

COPY deps.edn sieve.clj run.sh build.sh ./
RUN ./build.sh

ENTRYPOINT ["./run.sh"]
