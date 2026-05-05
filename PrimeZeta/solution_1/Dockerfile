FROM ubuntu:22.04 AS build

RUN apt-get update && apt-get install -y gcc libc6-dev curl && rm -rf /var/lib/apt/lists/*

WORKDIR /build

RUN curl -sL "https://github.com/murphsicles/zeta/raw/main/bin/zetac" -o /usr/local/bin/zetac && chmod +x /usr/local/bin/zetac

COPY src/prime.z runtime.c ./
RUN gcc -O2 -c runtime.c -o zeta_runtime_c.o && zetac prime.z -o prime

FROM ubuntu:22.04

COPY --from=build /build/prime /prime
COPY run.sh /

CMD ["/run.sh"]
