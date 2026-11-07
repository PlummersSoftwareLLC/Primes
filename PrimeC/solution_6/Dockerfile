# ---- build stage ----
FROM ubuntu:22.04 AS build

RUN apt-get update -qq \
 && apt-get install -y --no-install-recommends bash clang lld \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY PrimeC_extend.c .

ARG TARGETARCH
RUN set -eux; \
    CFLAGS="-march=native -mtune=native -pthread -O3 -ffast-math -fno-math-errno -fno-trapping-math -flto -std=c11 -fvectorize -fslp-vectorize"; \
    if [ "${TARGETARCH:-amd64}" = "arm64" ]; then \
      CFLAGS="-mcpu=native -pthread -O3 -ffast-math -fno-math-errno -fno-trapping-math -flto -std=c11 -fvectorize -fslp-vectorize"; \
    fi; \
    clang ${CFLAGS} -fuse-ld=lld PrimeC_extend.c -o primes_c_extend.exe -lm -lpthread

# ---- runtime stage ----
FROM ubuntu:22.04

WORKDIR /opt/app
COPY --from=build /opt/app/primes_c_extend.exe /opt/app/
COPY benchmark.sh .
RUN chmod +x benchmark.sh

ENTRYPOINT ["./benchmark.sh"]
CMD ["dummy", "-l", "1000000"]
