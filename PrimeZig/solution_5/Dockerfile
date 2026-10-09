FROM debian:bookworm-slim AS build

RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates curl xz-utils \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /tmp/zig-install
RUN arch="$(uname -m)" \
    && case "${arch}" in \
         x86_64) file="zig-x86_64-linux-0.17.0.tar.xz"; sum="1cbe9df9f27e6b78d14ccbca43b6703a404ef79ef1c463de901d7f088d4e2026" ;; \
         aarch64) file="zig-aarch64-linux-0.17.0.tar.xz"; sum="9e8d11661d4ae3bd57702a3832781e23ad151dde5798e16a5ccd503f65234ff8" ;; \
         *) echo "unsupported architecture: ${arch}" >&2; exit 1 ;; \
       esac \
    && curl -fsSL -o "${file}" "https://ziglang.org/download/0.17.0/${file}" \
    && printf '%s  %s\n' "${sum}" "${file}" > checksum \
    && sha256sum -c checksum \
    && tar -xf "${file}" \
    && mv "zig-${arch}-linux-0.17.0" /opt/zig

WORKDIR /opt/app
COPY build.zig ./
COPY src ./src
RUN /opt/zig/zig build -Doptimize=ReleaseFast --prefix /opt/app/out

FROM debian:bookworm-slim
WORKDIR /opt/app
COPY --from=build /opt/app/out/bin/prime-zig /opt/app/prime-zig
ENTRYPOINT ["/opt/app/prime-zig"]
