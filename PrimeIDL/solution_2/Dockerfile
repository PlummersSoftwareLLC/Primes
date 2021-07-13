FROM primeimages/gdl:1.0.0-rc3 AS build

COPY *.pro /opt/app/
WORKDIR /opt/app

ENTRYPOINT [ "gdl", "-quiet", "-e", "primeidl" ]
