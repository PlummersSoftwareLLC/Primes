FROM debian:trixie-slim

RUN apt-get update \
    && apt-get install -y --no-install-recommends godot3-server \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY primes.gd .

CMD ["godot3-server", "--no-window", "-s", "primes.gd"]
