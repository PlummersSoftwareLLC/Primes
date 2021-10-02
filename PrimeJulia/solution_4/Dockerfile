FROM julia:1.6-buster

WORKDIR /opt/app

COPY *.jl ./

ENTRYPOINT [ "julia", "-O3", "primes.jl" ]
