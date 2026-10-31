FROM alpine:3.24
WORKDIR /opt/app
COPY . .
RUN ./make.sh
ENTRYPOINT [ "./run.sh" ]
