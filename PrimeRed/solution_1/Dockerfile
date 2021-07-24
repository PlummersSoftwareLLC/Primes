FROM kskarthik/redlang:0.6.4
WORKDIR /opt/app
COPY PrimeRed.red .
RUN /usr/local/bin/red -c -r PrimeRed.red
ENTRYPOINT [ "./PrimeRed" ]
