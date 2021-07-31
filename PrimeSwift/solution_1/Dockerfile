FROM swift:latest

WORKDIR /opt/app

COPY ./PrimeSwift_1bitBool/ ./PrimeSwift_1bitBool/
COPY ./PrimeSwift_8bitBool/ ./PrimeSwift_8bitBool/

RUN swift build --configuration release -Xswiftc -O --package-path PrimeSwift_1bitBool
RUN swift build --configuration release -Xswiftc -O --package-path PrimeSwift_8bitBool

COPY run.sh .
RUN chmod a+x run.sh

ENTRYPOINT [ "./run.sh" ]
