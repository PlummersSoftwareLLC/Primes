FROM rustlang/rust@sha256:5bb8064b932c22a51677008d8454a95148801e6c4ae25d245f7248a4ea3c1944
WORKDIR /app
COPY . .
RUN rustc +nightly src/singlethreaded.rs -Copt-level=z -C target-cpu=native \
    && rustc +nightly src/multithreaded.rs -Copt-level=z -C target-cpu=native


ENTRYPOINT [ "bash", "run-by-docker.sh"]