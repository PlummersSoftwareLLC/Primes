FROM primeimages/opencomal:0.3.0

RUN apt-get update && apt-get install -y socat && apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app/

COPY opencomal.txt run.sh build.sh ./

RUN sh build.sh

ENTRYPOINT [ "sh", "run.sh" ]

