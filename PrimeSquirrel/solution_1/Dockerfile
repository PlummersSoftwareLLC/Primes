FROM ubuntu:22.04

RUN apt-get update -qq \
	&& apt-get install -y squirrel3 \
	&& apt-get clean \
	&& rm -rf /var/lib/apt/lists/* \
	&& mkdir -p /usr/local/squirrel
	

COPY ["prime.nut", "run.sh", "/usr/local/squirrel/"]

WORKDIR /usr/local/squirrel

ENTRYPOINT ["./run.sh"]
