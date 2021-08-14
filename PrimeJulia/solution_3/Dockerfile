FROM julia:1.6-buster

WORKDIR /opt/app

COPY primes_*.jl ./
COPY run.sh ./

ENTRYPOINT [ "sh", "run.sh" ]