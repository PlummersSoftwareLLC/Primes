FROM ubuntu:24.04 AS build

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
    curl gcc libc6-dev lsb-release wget gnupg ca-certificates && \
    rm -rf /var/lib/apt/lists/*

RUN wget -qO- https://apt.llvm.org/llvm-snapshot.gpg.key | tee /etc/apt/trusted.gpg.d/llvm.asc && \
    echo "deb http://apt.llvm.org/$(lsb_release -cs)/ llvm-toolchain-$(lsb_release -cs)-21 main" >> /etc/apt/sources.list && \
    apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y llvm-21 && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /build

RUN curl -sL "https://github.com/murphsicles/zeta/raw/main/bin/zetac" -o /usr/local/bin/zetac && chmod +x /usr/local/bin/zetac

COPY src/prime.z runtime.c ./
RUN gcc -O2 -c runtime.c -o zeta_runtime_c.o && zetac prime.z -o prime

FROM ubuntu:24.04

COPY --from=build /build/prime /prime
COPY run.sh /

CMD ["/run.sh"]
