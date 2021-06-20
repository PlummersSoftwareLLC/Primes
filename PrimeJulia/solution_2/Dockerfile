FROM julia:1.6.1-alpine3.13

WORKDIR /opt/app

COPY *.jl ./

ENTRYPOINT [ "julia", "Primes.jl" ]
