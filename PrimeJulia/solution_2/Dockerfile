FROM julia:1.13.3-trixie

WORKDIR /opt/app

COPY *.jl ./

ENTRYPOINT [ "julia", "Primes.jl" ]
