FROM rustlang/rust:nightly as build
WORKDIR /app
COPY . .
RUN rustc +nightly src/singlethreaded.rs -Copt-level=z -C target-cpu=native
RUN rustc +nightly src/multithreaded.rs -Copt-level=z -C target-cpu=native


ENTRYPOINT [ "bash", "run-by-docker.sh"]