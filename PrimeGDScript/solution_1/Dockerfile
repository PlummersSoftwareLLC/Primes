FROM alpine:3.14

# Variables
ENV GODOT_VERSION "3.3.2"

# Updates and installs
RUN apk update \
    && apk add --no-cache bash=5.1.4-r0 wget=1.21.1-r1

# Allow this to run Godot
RUN wget --progress=dot:giga https://github.com/sgerrand/alpine-pkg-glibc/releases/download/2.31-r0/glibc-2.31-r0.apk \
    && apk add --allow-untrusted --no-cache glibc-2.31-r0.apk

# Download Godot, version is set from variables
RUN wget --progress=dot:giga https://downloads.tuxfamily.org/godotengine/${GODOT_VERSION}/Godot_v${GODOT_VERSION}-stable_linux_headless.64.zip \
    && mkdir ~/.cache \
    && mkdir -p ~/.config/godot \
    && unzip Godot_v${GODOT_VERSION}-stable_linux_headless.64.zip \
    && mv Godot_v${GODOT_VERSION}-stable_linux_headless.64 /usr/local/bin/godot \
    && rm -f Godot_v${GODOT_VERSION}-stable_linux_headless.64.zip

COPY . /

CMD ["godot", "--no-window", "-s", "primes.gd"]