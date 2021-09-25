FROM primeimages/freebasic:1.07.3 AS build


WORKDIR /opt/app
COPY *.bas ./
COPY run.sh .
RUN chmod a+x run.sh && \
    find . -name 'prime_*.bas' -exec fbc {} -x {}.run \;

FROM ubuntu:18.04
COPY --from=build /opt/app/*.run /opt/app/
COPY --from=build /opt/app/run.sh /opt/app/
WORKDIR /opt/app

ENTRYPOINT [ "./run.sh" ]