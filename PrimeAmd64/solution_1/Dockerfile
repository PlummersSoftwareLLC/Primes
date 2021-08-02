FROM alpine:3.13
WORKDIR /opt/app
COPY *.sh *.dmp ./
RUN chmod +x *.sh
RUN ./make.sh
ENTRYPOINT [ "./run.sh" ]
