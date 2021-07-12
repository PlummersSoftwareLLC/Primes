FROM pvargacl/oracle-xe-18.4.0

ENV ORACLE_PWD=oracle

WORKDIR /opt/app

COPY *.sql ./
COPY run.sh ./

USER oracle

ENTRYPOINT ["bash"]
