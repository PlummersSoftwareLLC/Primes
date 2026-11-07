FROM ubuntu:24.04 AS build

# Verilator 4.210 is built from source. Ubuntu 24.04 packages Verilator 5.020,
# whose scheduler makes this test bench run about 7 times slower.
ARG VERILATOR_VERSION=v4.210

RUN apt-get update \
    && apt-get install -y autoconf bison ca-certificates flex g++ git help2man libfl-dev make perl python3 zlib1g-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Only the optimized verilator_bin is built, and it is used in place. The sed
# adds an include that GCC 11+ no longer pulls in transitively.
WORKDIR /opt/verilator
RUN git clone --depth 1 --branch "${VERILATOR_VERSION}" https://github.com/verilator/verilator . \
    && sed -i 's/^#include <algorithm>$/#include <algorithm>\n#include <memory>/' src/V3Const.cpp \
    && autoconf \
    && ./configure \
    && make -j"$(nproc)" -C src opt

ENV VERILATOR_ROOT=/opt/verilator
ENV PATH="/opt/verilator/bin:${PATH}"

WORKDIR /work
COPY *.v .
COPY *.cpp .
RUN verilator --cc prime_number_core.v --exe prime_number_core_tb.cpp --top prime_number_core
WORKDIR /work/obj_dir
RUN make -j -f Vprime_number_core.mk Vprime_number_core

FROM ubuntu:24.04 AS prime
COPY --from=build /work/obj_dir/Vprime_number_core /usr/local/bin/Vprime_number_core

ENTRYPOINT ["Vprime_number_core"]
