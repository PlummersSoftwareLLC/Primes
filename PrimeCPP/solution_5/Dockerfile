# ---- build stage ----
FROM ubuntu:22.04 AS build

RUN apt-get update -qq \
 && apt-get install -y bash clang lld

WORKDIR /opt/app
COPY *.cpp .

# Build with the same flags as run.sh to enable vectorization
# across Mac and Linux. On ARM64, use -mcpu=native instead of -march/-mtune.
ARG TARGETARCH
RUN set -eux; \
    CXXFLAGS="-march=native -mtune=native -pthread -O3 -ffast-math -fno-math-errno -fno-trapping-math -flto -std=c++17"; \
    if [ "${TARGETARCH:-amd64}" = "arm64" ]; then \
      CXXFLAGS="-mcpu=native -pthread -O3 -ffast-math -fno-math-errno -fno-trapping-math -flto -std=c++17"; \
    fi; \
    clang++ ${CXXFLAGS} -fuse-ld=lld PrimeCPP_array.cpp -o primes_array.exe

# ---- runtime stage ----
FROM ubuntu:22.04

WORKDIR /opt/app
COPY --from=build /opt/app/primes_array.exe /opt/app/
COPY benchmark.sh .
RUN chmod +x benchmark.sh

ENTRYPOINT ["./benchmark.sh"]
CMD ["dummy", "-l", "1000000"]
