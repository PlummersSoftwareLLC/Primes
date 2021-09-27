FROM ubuntu:20.04

SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN apt-get update \
    && apt-get install -y curl busybox \
    && curl -sL https://github.com/wren-lang/wren-cli/releases/download/0.3.0/wren_cli-linux-0.3.0.zip | busybox unzip - -d / \
    && cp /wren_cli-linux-0.3.0/wren_cli /usr/local/bin \
    && rm -rf /wren_cli-linux-0.3.0 \
    && chmod a+x /usr/local/bin/wren_cli 

WORKDIR /opt/app
COPY primes.wren .

ENTRYPOINT [ "/usr/local/bin/wren_cli", "primes.wren" ]
