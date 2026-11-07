# Ubuntu 24.04 no longer packages squirrel3; Debian 13 does (same 3.1 release).
FROM debian:trixie-slim

RUN apt-get update -qq \
	&& apt-get install -y squirrel3 \
	&& apt-get clean \
	&& rm -rf /var/lib/apt/lists/* \
	&& mkdir -p /usr/local/squirrel
	

COPY ["prime.nut", "run.sh", "/usr/local/squirrel/"]

WORKDIR /usr/local/squirrel

ENTRYPOINT ["./run.sh"]
