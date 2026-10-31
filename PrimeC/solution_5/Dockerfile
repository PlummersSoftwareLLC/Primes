FROM alpine:3.24

# Install necessary dependencies
RUN apk add --no-cache build-base mimalloc2-dev clang openmp-dev bash && \
    rm -rf /var/cache/apk/*

# Set mimalloc as the default memory allocator using LD_PRELOAD
ENV LD_PRELOAD=/usr/lib/libmimalloc.so

WORKDIR /home/sieve

# Copy all shell scripts, C source, and header files
COPY src/ ./src/
COPY sieve ./sieve

# Ensure the bin directory exists
RUN mkdir -p bin dev/build && \
    chmod +x ./sieve && \
    sed -i 's/\r$//' ./sieve

RUN ./sieve compileall

ENTRYPOINT [ "./sieve" ]
