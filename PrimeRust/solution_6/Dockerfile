FROM rustlang/rust:nightly as build
WORKDIR /app
COPY . .
RUN rustc +nightly src/main.rs -Copt-level=z -C target-cpu=native


ENTRYPOINT [ "./main" ]
