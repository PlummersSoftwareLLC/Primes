FROM ubuntu:21.04 AS build

RUN apt update -qq && apt install -y gcc g++ clang make

WORKDIR /opt/app

COPY Makefile .
COPY *.cpp .
COPY *.hpp .

RUN make

ENTRYPOINT [ "/usr/bin/make", "run" ]
