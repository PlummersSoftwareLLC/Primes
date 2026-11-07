FROM clojure:temurin-25-tools-deps-1.12.6.1673-alpine
WORKDIR /opt/app
COPY PrimeClojure.clj .
ENTRYPOINT [ "clojure", "-M", "PrimeClojure.clj" ]
