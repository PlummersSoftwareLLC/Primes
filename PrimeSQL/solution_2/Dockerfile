FROM mariadb:11.8-noble

WORKDIR /opt/app

COPY *.sql run.sh ./

USER mysql
ENTRYPOINT ["./run.sh"]
