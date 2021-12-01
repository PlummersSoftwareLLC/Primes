FROM alpine:3.13 as builder

RUN apk update && \
    apk --no-cache add \
        curl \
        xz

WORKDIR /deps
RUN curl https://ziglang.org/download/0.8.0/zig-linux-"$(uname -m)"-0.8.0.tar.xz  -O && \
    tar xf zig-linux-"$(uname -m)"-0.8.0.tar.xz && \
    mv zig-linux-"$(uname -m)"-0.8.0 local/

FROM alpine:3.13
COPY --from=builder /deps/local/ /deps/local/
RUN ln -s /deps/local/zig /usr/bin/zig

WORKDIR /opt/app
COPY . .
RUN zig build -Drelease-fast

ENTRYPOINT [ "./zig-out/bin/PrimeZig" ]