FROM clojure:temurin-25-tools-deps-1.12.6.1673-trixie

WORKDIR /primes

COPY . ./
RUN ./build.sh

ENTRYPOINT ["./run.sh"]
