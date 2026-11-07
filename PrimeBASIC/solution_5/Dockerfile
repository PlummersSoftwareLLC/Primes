# pitybas, the TI-BASIC interpreter used here, only runs on Python 2.7. No supported distro
# packages Python 2.7 anymore, so it is built from the official source release.
FROM debian:trixie-slim AS build

ARG PYTHON_VERSION=2.7.18
ARG PYTHON_SHA256=b62c0e7937551d0cc02b8fd5cb0f544f9405bafc9a54d3808ed4594812edef43

RUN apt-get update \
    && apt-get install -y --no-install-recommends build-essential \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /tmp
ADD https://www.python.org/ftp/python/${PYTHON_VERSION}/Python-${PYTHON_VERSION}.tar.xz python.tar.xz
RUN echo "${PYTHON_SHA256}  python.tar.xz" | sha256sum -c - \
    && tar xJf python.tar.xz

WORKDIR /tmp/Python-${PYTHON_VERSION}
RUN ./configure --prefix=/usr/local \
    && make -j"$(nproc)" \
    && make altinstall

FROM debian:trixie-slim

COPY --from=build /usr/local /usr/local
ADD https://github.com/rzuckerm/pitybas.git#v2025.03.08.2 /opt/pitybas
ENV PYTHONPATH=/opt/pitybas

WORKDIR /opt/app
COPY *.8xp.txt run*.sh ./
ENTRYPOINT [ "./run_primes.sh" ]
