FROM opensuse/leap:latest

ARG DYALOG_RELEASE=18.0
ARG DYALOG_VERSION=${DYALOG_RELEASE}.40684
ARG BUILDTYPE=minimal
ARG RPMFILE=https://www.dyalog.com/uploads/php/download.dyalog.com/download.php?file=${DYALOG_RELEASE}/linux_64_${DYALOG_VERSION}_unicode.x86_64.rpm

ADD ${RPMFILE} /tmp/dyalog.rpm

RUN zypper install --allow-unsigned-rpm -y /tmp/dyalog.rpm

WORKDIR /opt/app

COPY PrimeSieveAPL.apln .
COPY bench.sh .

ENTRYPOINT ["./bench.sh"]
