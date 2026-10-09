# ---------- 1. Build stage ----------
FROM debian:bookworm AS build

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        gcc \
        make \
        libc6-dev && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY . .

RUN chmod +x build.sh && ./build.sh

# ---------- 2. Runtime stage ----------
FROM debian:bookworm-slim AS runtime

WORKDIR /app

# Copy binaries and run script
COPY --from=build /app/tacitvs_st .
COPY --from=build /app/tacitvs_mt .
COPY run.sh .

RUN chmod +x run.sh

ENTRYPOINT ["./run.sh"]
