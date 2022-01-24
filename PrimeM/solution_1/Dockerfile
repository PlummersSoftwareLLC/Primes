FROM yottadb/yottadb-base:r1.32

ENV ydb_dist=/opt/yottadb/current
ENV ydb_routines="/opt/app $ydb_dist"

WORKDIR /opt/app
COPY *.m run.sh ./
RUN "$ydb_dist/yottadb" ./*.m

ENTRYPOINT [ "sh", "run.sh" ]
