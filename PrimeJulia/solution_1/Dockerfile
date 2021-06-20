FROM julia:1.6-alpine3.13

WORKDIR /opt/app

COPY *.jl ./

ENTRYPOINT [ "julia", "PrimeSieveJulia.jl" ]