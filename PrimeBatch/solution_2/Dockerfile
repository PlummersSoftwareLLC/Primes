# not ideal to use wine, but no other choice on unix
ARG nmax=1000000
ARG vhd=n
FROM scottyhardy/docker-wine:stable-6.0.1

WORKDIR /opt/app
COPY *.cmd ./
 
ENTRYPOINT [ "wine", "cmd", "/c", "PrimeFiles.cmd", "NMAX=$nmax", "VHD=$vhd" ]
