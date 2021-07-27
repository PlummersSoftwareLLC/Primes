FROM ubuntu:20.04
RUN apt-get update -y && \
    apt-get install smlnj -y
WORKDIR /app
COPY sml_primes.sml sml_primes.sml
COPY build.sml build.sml
RUN sml build.sml
CMD ["sml", "@SMLload=primes.x86-linux"]
