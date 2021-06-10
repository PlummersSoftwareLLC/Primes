FROM alpine:3.13

RUN apk add --no-cache R
#RUN apk add --no-cache R-dev
# below are required to make the format fit the drag-race format
RUN apk add --no-cache bash
RUN apk add --no-cache sed

WORKDIR /opt/app

COPY primes.R .
COPY runR.sh .

ENTRYPOINT ["./runR.sh" ]
