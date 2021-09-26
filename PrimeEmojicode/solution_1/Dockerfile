FROM ubuntu:20.04 AS build

# hadolint ignore=DL3008,DL3015
RUN apt-get update -qq \
    && apt-get install -y curl rsync libncurses5 build-essential

WORKDIR /opt/emojicode

# hadolint ignore=DL4006
RUN curl -sL https://github.com/emojicode/emojicode/releases/download/v1.0-beta.2/Emojicode-1.0-beta.2-Linux-x86_64.tar.gz  | tar zxv --strip-components 1 \
    && yes | ./install.sh

WORKDIR /opt/app
COPY primes.emojic .
RUN emojicodec primes.emojic

FROM ubuntu:20.04

COPY --from=build /opt/app/primes /usr/local/bin

ENTRYPOINT [ "primes" ]
