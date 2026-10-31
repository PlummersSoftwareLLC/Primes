# ballerina docker images only support amd64
FROM ballerina/ballerina:2201.14.1

WORKDIR /app

COPY PrimeBal.bal .
RUN bal build PrimeBal.bal

ENTRYPOINT [ "bal", "run", "PrimeBal.jar" ]
