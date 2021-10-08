FROM ubuntu:focal AS build
RUN apt-get update && apt-get -y install gdc
WORKDIR /app
COPY . .
RUN gdc -o prime-d -O2 /app/source/app.d

FROM ubuntu:focal
RUN apt-get update && apt-get -y install libgphobos1 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
COPY --from=build /app/prime-d /app/prime-d
ENTRYPOINT [ "/app/prime-d" ]