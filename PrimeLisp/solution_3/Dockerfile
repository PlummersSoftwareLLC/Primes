FROM debian:bookworm-slim AS build

RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates gcc git libc6-dev make sbcl \
    && rm -rf /var/lib/apt/lists/*

# Use the distribution SBCL to bootstrap the required release from source.
WORKDIR /tmp
RUN git clone --depth 1 --branch sbcl-2.6.8 https://github.com/sbcl/sbcl.git \
    && test "$(git -C sbcl rev-parse HEAD)" = "62f4bd103264bf1c92fb4fc68305b8307fdd06fb"
WORKDIR /tmp/sbcl
RUN SBCL_BUILDING_RELEASE_FROM=62f4bd103264bf1c92fb4fc68305b8307fdd06fb \
    sh make.sh --prefix=/opt/sbcl \
    && INSTALL_ROOT=/opt/sbcl sh install.sh

FROM debian:bookworm-slim
COPY --from=build /opt/sbcl /opt/sbcl
ENV PATH="/opt/sbcl/bin:${PATH}"
WORKDIR /opt/app
COPY *.lisp run.sh author.txt ./
ENTRYPOINT ["./run.sh"]
