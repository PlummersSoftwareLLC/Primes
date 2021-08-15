FROM alpine:3.14
WORKDIR /opt/app
COPY . .
RUN ./make.sh
ENTRYPOINT [ "./run.sh" ]
