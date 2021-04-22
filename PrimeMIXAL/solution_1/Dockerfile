FROM ubuntu:18.04

RUN apt-get update -qq \
    && apt-get install -y mdk time

WORKDIR /opt/app
COPY *.mixal .
COPY runprime.sh .

RUN mixasm prime
RUN chmod a+x runprime.sh

ENTRYPOINT [ "/opt/app/runprime.sh" ]