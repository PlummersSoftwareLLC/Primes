FROM rfvanbergen/freebasic:1.07.3 AS build

WORKDIR /opt/app
COPY runprimes.sh .
RUN chmod a+x runprimes.sh
COPY *.bas /opt/app/
RUN find . -name 'prime_*.bas' -exec fbc {} \;

FROM ubuntu:18.04
COPY --from=build /opt/app/* /opt/app/

ENTRYPOINT [ "/opt/app/runprimes.sh" ]