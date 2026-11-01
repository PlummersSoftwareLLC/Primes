FROM rust:1.99-slim-trixie
WORKDIR /app
COPY . .
RUN rustc src/singlethreaded.rs -Copt-level=z -C target-cpu=native \
    && rustc src/multithreaded.rs -Copt-level=z -C target-cpu=native


ENTRYPOINT [ "bash", "run-by-docker.sh"]