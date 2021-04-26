FROM rfvanbergen/freebasic:1.07.3

WORKDIR /opt/app
COPY runprimes.sh .
RUN chmod a+x runprimes.sh
COPY *.bas /opt/app/
RUN find . -name 'prime_*.bas' -exec fbc {} \;

ENTRYPOINT [ "/opt/app/runprimes.sh" ]