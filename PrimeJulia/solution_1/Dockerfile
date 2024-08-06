FROM julia:1.10-alpine

WORKDIR /opt/app

COPY *.jl ./

ENTRYPOINT [ "julia", "PrimeSieveJulia.jl" ]