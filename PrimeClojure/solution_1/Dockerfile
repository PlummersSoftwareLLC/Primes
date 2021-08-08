FROM clojure:openjdk-17-tools-deps-1.10.3.933-alpine
WORKDIR /opt/app
COPY PrimeClojure.clj .
ENTRYPOINT [ "clojure", "-M", "PrimeClojure.clj" ]
