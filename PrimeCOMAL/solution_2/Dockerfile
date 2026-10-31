FROM ubuntu:24.04

# OpenCOMAL is not packaged, so build it from source, as the former
# primeimages/opencomal image did.
ARG OPENCOMAL_VERSION=v0.3.0

RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y build-essential flex bison libncurses-dev git locales bash libreadline-dev socat \
    && locale-gen ga_IE.utf8 && locale-gen ga_IE@euro && update-locale LANG=ga_IE.utf8 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# The OpenCOMAL makefile asks ncursesw5-config for its ncurses flags. Ubuntu 24.04
# only ships ncursesw6-config, so pass the (identical) flags explicitly.
RUN git clone --depth 1 --branch "${OPENCOMAL_VERSION}" https://github.com/poldy/OpenCOMAL.git /opt/OpenCOMAL \
    && make -C /opt/OpenCOMAL \
        TARG_CFLAGS="$(ncursesw6-config --cflags)" \
        TARG_LDFLAGS="-lreadline -lm $(ncursesw6-config --libs)"

ENV PATH="/opt/OpenCOMAL/bin:$PATH"

WORKDIR /opt/app/

COPY opencomal.txt run.sh build.sh ./

RUN sh build.sh

ENTRYPOINT [ "sh", "run.sh" ]
