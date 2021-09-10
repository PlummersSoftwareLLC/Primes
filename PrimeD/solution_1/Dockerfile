FROM ubuntu:focal AS build
RUN apt-get update && apt-get --no-install-recommends -y install gdc=4:10.0-1ubuntu2
WORKDIR /app
COPY . .
RUN gdc -o prime-d -O2 /app/source/app.d

FROM ubuntu:focal
RUN apt-get update && apt-get --no-install-recommends -y install libgphobos1=10.3.0-1ubuntu1~20.04 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
COPY --from=build /app/prime-d /app/prime-d
ENTRYPOINT [ "/app/prime-d" ]