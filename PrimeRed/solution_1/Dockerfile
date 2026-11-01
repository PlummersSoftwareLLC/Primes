FROM debian:trixie-slim

# Red is a 32-bit (i386) program. The toolchain binary is the official 0.6.6 release build;
# Red cannot be built from source without another opaque binary (Rebol 2).
ARG RED_SHA256=95c75a49f8b3d15b8ae1ddf10f9589bc0fd0eecf84d432bad163191f900cb23c

RUN dpkg --add-architecture i386 \
    && apt-get update \
    && apt-get install -y --no-install-recommends libc6:i386 libcurl4t64:i386 libgdk-pixbuf-2.0-0:i386 \
    && rm -rf /var/lib/apt/lists/*

ADD --chmod=755 https://static.red-lang.org/dl/linux/red-toolchain-066 /usr/local/bin/red
RUN echo "${RED_SHA256}  /usr/local/bin/red" | sha256sum -c -

WORKDIR /opt/app
COPY PrimeRed.red .
# The toolchain has to be invoked by its full path to find its own encapsulated data, and it
# exits with status 0 when compilation fails, hence the explicit check. Red 0.6.6 no longer
# accepts -c together with -r; -r on its own is the release mode build that -c -r produced.
# The compiler dlopens a helper library that needs an executable stack, which glibc 2.41+
# refuses unless told otherwise.
RUN GLIBC_TUNABLES=glibc.rtld.execstack=2 /usr/local/bin/red -r PrimeRed.red \
    && test -x PrimeRed
ENTRYPOINT [ "./PrimeRed" ]
