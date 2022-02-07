FROM clojure:openjdk-18-tools-deps

WORKDIR /primes

COPY deps.edn sieve.clj run.sh ./
RUN mkdir classes && clojure -M -e "(compile 'sieve)" && clojure -M:uberjar --main-class sieve

ENTRYPOINT ["./run.sh"]
