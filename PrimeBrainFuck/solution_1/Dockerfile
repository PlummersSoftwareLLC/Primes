# container for building
FROM ubuntu:24.04 AS build

# install tools
RUN apt-get update \
    && apt-get install -y lsb-release wget software-properties-common git clang cmake

# set clang as default compiler for C and C++
ENV CC=/usr/bin/clang \
    CXX=/usr/bin/clang++

# clone custom language interpreter, pinned to the latest commit (2021-08-02)
ARG BF_INTERPRETER_COMMIT=8e1cba6e7fd1b60b5c0f658ea7344befda718e98
RUN git clone https://github.com/ThatAquarel/BrainF-ck-Interpreter \
    && git -C BrainF-ck-Interpreter checkout "${BF_INTERPRETER_COMMIT}" \
    && mkdir BrainF-ck-Interpreter/release/
WORKDIR /BrainF-ck-Interpreter/release/

# build interpreter
RUN cmake -DCMAKE_BUILD_TYPE=Release .. \
    && make \
    && cp BrainF_ck_Interpreter brainfuck

# build prime sieve caller
WORKDIR /opt/app/
COPY *.cpp *.b ./
RUN clang++ -Ofast -std=c++17 PrimeBrainFuck.cpp -oPrimeBrainFuck

# container for running built binaries
FROM ubuntu:24.04

# copy binaries from build container to current
COPY --from=build /BrainF-ck-Interpreter/release/brainfuck /usr/local/bin
COPY --from=build /opt/app/PrimeBrainFuck /usr/local/bin
COPY --from=build /opt/app/PrimeBrainFuck.b /opt/app/

# run
ENTRYPOINT [ "PrimeBrainFuck", "/opt/app/PrimeBrainFuck.b" ]
