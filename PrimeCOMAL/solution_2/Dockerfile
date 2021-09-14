FROM primeimages/opencomal:0.3.0

RUN apt-get update && apt-get install -y socat

WORKDIR /opt/app/

COPY opencomal.txt run.sh build.sh .

RUN sh build.sh

ENTRYPOINT [ "sh", "run.sh" ]

