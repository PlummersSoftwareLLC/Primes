FROM alpine:3.13

RUN echo "@testing http://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories
RUN apk add --no-cache build-base bash python3 python3-dev py3-pip py3-numpy py3-pandas@testing

WORKDIR /opt/tools
# COPY requirements.txt .
# RUN pip3 install -r requirements.txt

COPY bin/ bin/

ENV PATH=/opt/tools/bin:$PATH