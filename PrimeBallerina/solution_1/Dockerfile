# ballerina docker images only support amd64
FROM ballerina/ballerina:swan-lake-beta2

WORKDIR /app

COPY PrimeBal.bal .

ENTRYPOINT [ "bal", "run", "PrimeBal.bal" ]