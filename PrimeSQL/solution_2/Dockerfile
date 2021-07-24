FROM mariadb:10.6.2

WORKDIR /opt/app

COPY *.sql ./
COPY run.sh ./

USER mysql
ENTRYPOINT ["./run.sh"]
