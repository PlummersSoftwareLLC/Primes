FROM mariadb:10.6.2

WORKDIR /opt/app
#WORKDIR /docker-entrypoint-initdb.d
COPY docker-entrypoint.sh /opt/app/

ENV MARIADB_ROOT_PASSWORD=primes

COPY *.sql /docker-entrypoint-initdb.d/


ENTRYPOINT ["/opt/app/docker-entrypoint.sh", "mariadbd"]
