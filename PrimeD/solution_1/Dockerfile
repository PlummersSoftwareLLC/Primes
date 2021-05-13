FROM ubuntu:hirsute
RUN apt-get update && apt-get -y install gdc
WORKDIR /app
ADD . /app
RUN gdc -o prime-d -O2 /app/source/app.d

FROM ubuntu:hirsute
RUN apt-get update && apt-get -y install libgphobos1
COPY --from=0 /app/prime-d /app/prime-d
ENTRYPOINT [ "/app/prime-d" ]