FROM clojure:openjdk-18-tools-deps

WORKDIR /primes

COPY . ./
RUN ./build.sh

ENTRYPOINT ["./run.sh"]
