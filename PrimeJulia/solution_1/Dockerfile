FROM julia:1-alpine

WORKDIR /opt/app

COPY *.jl ./

ENTRYPOINT [ "julia", "PrimeSieveJulia.jl" ]