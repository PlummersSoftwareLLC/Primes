FROM swift:latest

WORKDIR /opt/app

COPY PrimeSwift_1bitBool/ ./PrimeSwift_1bitBool/
COPY PrimeSwift_8bitBool/ ./PrimeSwift_8bitBool/
COPY run.sh .
RUN chmod a+x run.sh

ENTRYPOINT [ "./run.sh" ]
