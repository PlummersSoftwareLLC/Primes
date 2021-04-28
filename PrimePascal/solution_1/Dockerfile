FROM ubuntu:18.04

RUN apt-get update -qq \
    && apt-get install -y fpc

WORKDIR /opt/app
COPY *.pas .

RUN fpc prime

ENTRYPOINT [ "./prime" ]