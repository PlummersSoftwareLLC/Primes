FROM ubuntu:18.04

RUN apt-get update -qq && apt-get install --no-install-recommends -y chezscheme=9.5+dfsg-2build2  \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app
COPY . .

ENTRYPOINT [ "./run.ss" ]