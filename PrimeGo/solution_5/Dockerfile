FROM golang:1.27 AS build
WORKDIR /source

# The vector loop uses simd/archsimd, which only exists with this experiment switched on.
ENV GOEXPERIMENT=simd CGO_ENABLED=0

COPY go.mod *.go ./
COPY gen/ ./gen/
# generate the marking loops (markers_gen.go), then build
RUN go generate && go build -o /app/primes .

FROM alpine:3.24
COPY --from=build /app/primes /app/primes
ENTRYPOINT ["/app/primes"]
