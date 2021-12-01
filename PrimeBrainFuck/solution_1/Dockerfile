# container for building
FROM ubuntu:18.04 AS build

# install tools
RUN apt-get update \
    && apt-get install -y lsb-release wget software-properties-common git

# install clang-12 for C++ standard 17
RUN wget https://apt.llvm.org/llvm.sh \
    && chmod +x llvm.sh \
    && ./llvm.sh 12

# set clang as default compiler for C and C++
ENV CC=/usr/bin/clang-12 \
    CXX=/usr/bin/clang++-12

# install latest version of cmake
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc 2>/dev/null | apt-key add - \
    && apt-add-repository 'deb https://apt.kitware.com/ubuntu/ bionic main' \
    && apt-get update \
    && apt-get install -y cmake

# clone custom language interpreter
RUN git clone https://github.com/ThatAquarel/BrainF-ck-Interpreter \
    && mkdir BrainF-ck-Interpreter/release/
WORKDIR /BrainF-ck-Interpreter/release/

# build interpreter
RUN cmake -DCMAKE_BUILD_TYPE=Release .. \
    && make \
    && cp BrainF_ck_Interpreter brainfuck

# build prime sieve caller
WORKDIR /opt/app/
COPY *.cpp *.b ./
RUN clang++-12 -Ofast -std=c++17 PrimeBrainFuck.cpp -oPrimeBrainFuck

# container for running built binaries
FROM ubuntu:18.04

# copy binaries from build container to current
COPY --from=build /BrainF-ck-Interpreter/release/brainfuck /usr/local/bin
COPY --from=build /opt/app/PrimeBrainFuck /usr/local/bin
COPY --from=build /opt/app/PrimeBrainFuck.b /opt/app/

# run
ENTRYPOINT [ "PrimeBrainFuck", "/opt/app/PrimeBrainFuck.b" ]
