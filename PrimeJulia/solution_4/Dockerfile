FROM julia:1

WORKDIR /opt/app

COPY *.jl ./

ENTRYPOINT [ "julia", "-O3", "primes.jl" ]
