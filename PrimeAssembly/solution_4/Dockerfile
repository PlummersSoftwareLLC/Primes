# ---------- 1. Build stage ----------
FROM debian:bookworm AS build

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        nasm \
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

# Copy both binaries
COPY --from=build /app/cwager_x64ff_mt .
COPY --from=build /app/cwager_x64ff_mt_extreme .

# Copy runner
COPY run.sh .

RUN chmod +x run.sh

ENTRYPOINT ["./run.sh"]
