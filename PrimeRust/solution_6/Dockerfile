FROM rustlang/rust:nightly as build
WORKDIR /app
COPY . .
RUN rustc +nightly src/main.rs -Copt-level=3 -C target-cpu=native


ENTRYPOINT [ "./main" ]
