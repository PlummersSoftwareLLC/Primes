FROM ubuntu:18.04

RUN chmod 777 -R /tmp && chmod o+t -R /tmp
RUN apt-get update -qq \
    && apt-get install -y mdk dos2unix time

WORKDIR /opt/app
COPY *.mixal .
COPY runprime.sh .

RUN find . -name *.mixal -exec dos2unix {} \; 
RUN mixasm prime
RUN chmod a+x runprime.sh

#ENTRYPOINT exec `which time` -f '%e' mixvm -r prime 2>/opt/app/primetime.txt | tr '[:upper:]' '[:lower:]' | sed -n "s/<time>/`cat \/opt\/app\/primetime.txt`/p"
ENTRYPOINT exec /opt/app/runprime.sh
