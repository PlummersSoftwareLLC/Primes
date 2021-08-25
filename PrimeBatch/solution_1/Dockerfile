# not ideal to use wine, but no other choice on unix
ARG workers=1
ARG sieveSize=1000000
FROM scottyhardy/docker-wine:stable-6.0.1

WORKDIR /opt/app
COPY *.bat ./
 
ENTRYPOINT [ "wine", "cmd", "/c", "main.bat", "/workers:$workers", "/sieveSize:$sieveSize" ]
