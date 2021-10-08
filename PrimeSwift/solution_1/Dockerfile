FROM swift:5.4.2

WORKDIR /opt/app

COPY . .

RUN swift build --configuration release -Xswiftc -O --package-path PrimeSwift_1bit_u8 && \
    swift build --configuration release -Xswiftc -O --package-path PrimeSwift_1bitStriped_u8 && \
    swift build --configuration release -Xswiftc -O --package-path PrimeSwift_8bitBool

ENTRYPOINT [ "./run.sh" ]