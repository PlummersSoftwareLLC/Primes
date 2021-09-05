FROM swift:5.4.2

WORKDIR /opt/app

COPY ./PrimeSwift_1bit_u8/ ./PrimeSwift_1bit_u8/
COPY ./PrimeSwift_1bitStriped_u8/ ./PrimeSwift_1bitStriped_u8/
COPY ./PrimeSwift_8bitBool/ ./PrimeSwift_8bitBool/
COPY run.sh .

RUN swift build --configuration release -Xswiftc -O --package-path PrimeSwift_1bit_u8 && \
    swift build --configuration release -Xswiftc -O --package-path PrimeSwift_1bitStriped_u8 && \
    swift build --configuration release -Xswiftc -O --package-path PrimeSwift_8bitBool && \
    chmod a+x run.sh

ENTRYPOINT [ "./run.sh" ]
