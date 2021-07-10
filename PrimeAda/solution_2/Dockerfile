FROM fedora:35 AS build

RUN yum update -y
RUN yum install -y yum-utils fedora-gnat-project-common gprbuild gcc-gnat

RUN mkdir -p /opt/app/src
WORKDIR /opt/app/src

WORKDIR /opt/app
COPY src/prime_sieves.ad[sb] src/prime_sieves_imp.ad[sb] src/sieves.adb src/
COPY ./optimised.adc ./prime_sieve.gpr ./

RUN /usr/bin/gprbuild -p -XMODE=optimised

ENTRYPOINT ["/opt/app/sieves"]
