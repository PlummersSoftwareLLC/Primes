FROM swift:5.4.2

WORKDIR /opt/app

COPY ./PrimeSwift_1bitBool_64bit/ ./PrimeSwift_1bitBool_64bit/
COPY ./PrimeSwift_1bitBool_32bit/ ./PrimeSwift_1bitBool_32bit/
COPY ./PrimeSwift_8bitBool/ ./PrimeSwift_8bitBool/
COPY run.sh .

RUN swift build --configuration release -Xswiftc -O --package-path PrimeSwift_1bitBool_64bit && \
    swift build --configuration release -Xswiftc -O --package-path PrimeSwift_1bitBool_32bit && \
    swift build --configuration release -Xswiftc -O --package-path PrimeSwift_8bitBool && \
    chmod a+x run.sh

ENTRYPOINT [ "./run.sh" ]
