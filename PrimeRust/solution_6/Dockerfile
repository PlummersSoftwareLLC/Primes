FROM rustlang/rust:nightly as build
WORKDIR /app
COPY . .
RUN rustc +nightly src/main.rs -Copt-level=z


ENTRYPOINT [ "./main" ]