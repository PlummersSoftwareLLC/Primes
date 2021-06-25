FROM alpine:3.13

RUN apk add --no-cache curl libc6-compat \
    && curl -sL https://github.com/wren-lang/wren-cli/releases/download/0.3.0/wren_cli-linux-0.3.0.zip | unzip -d / - \
    && cp /wren_cli-linux-0.3.0/wren_cli /usr/local/bin \
    && chmod a+x /usr/local/bin/wren_cli

WORKDIR /opt/app
COPY *.wren .

ENTRYPOINT [ "wren-cli", "primes.wren" ]