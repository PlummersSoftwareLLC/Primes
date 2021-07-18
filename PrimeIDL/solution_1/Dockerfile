FROM primeimages/gdl:1.0.0-rc3 AS build

WORKDIR /opt/app

COPY *.pro ./
COPY run.sh .

RUN chmod a+x run.sh

ENTRYPOINT [ "./run.sh" ]
