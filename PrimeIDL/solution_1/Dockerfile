FROM fedora

RUN yum update -q -y \
    && yum install -q -y gdl

COPY *.pro /opt/app/
WORKDIR /opt/app

ENTRYPOINT [ "gdl", "-quiet", "-e", "primeidl" ]