FROM julia:1.6-buster

WORKDIR /opt/app

COPY Project.toml ./
COPY *.jl ./

COPY src/ src/

ENTRYPOINT [ "julia", "--project=.", "main.jl" ]
