FROM ubuntu:24.04 AS odin

ARG ODIN_VERSION=dev-2025-12
ARG ODIN_ARCHIVE=odin-linux-amd64-${ODIN_VERSION}.tar.gz
ARG ODIN_URL=https://github.com/odin-lang/Odin/releases/download/${ODIN_VERSION}/${ODIN_ARCHIVE}

RUN apt-get update \
	&& apt-get install -y --no-install-recommends ca-certificates curl clang lld \
	&& rm -rf /var/lib/apt/lists/*

RUN mkdir -p /opt/odin \
	&& curl -fsSL "$ODIN_URL" -o /tmp/odin.tar.gz \
	&& tar -xzf /tmp/odin.tar.gz -C /opt/odin --strip-components=1 \
	&& rm /tmp/odin.tar.gz

ENV ODIN_ROOT=/opt/odin
ENV PATH="/opt/odin:${PATH}"

RUN odin version


FROM odin AS build

WORKDIR /opt/app

COPY *.odin ./
RUN odin build . -o:speed -no-bounds-check -out:primes-odin-1


FROM ubuntu:24.04 AS runtime

WORKDIR /opt/app

COPY --from=build /opt/app/primes-odin-1 primes-odin-1

ENTRYPOINT ["./primes-odin-1"]
