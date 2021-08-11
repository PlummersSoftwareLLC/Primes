# not ideal to use wine, but no other choice on unix
FROM scottyhardy/docker-wine:latest

WORKDIR /opt/app
COPY *.bat ./
 
ENTRYPOINT [ "wine", "cmd", "/c", "main.bat" ]
