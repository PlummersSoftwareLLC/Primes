FROM swift:6.3.3 AS build

WORKDIR /opt/app

COPY . .

RUN swift build -v --configuration release -Xswiftc -O -Xswiftc -whole-module-optimization --package-path PrimeSwift_1bit_u8 && \
    swift build -v --configuration release -Xswiftc -O -Xswiftc -whole-module-optimization --package-path PrimeSwift_1bitStriped_u8 && \
    swift build -v --configuration release -Xswiftc -O -Xswiftc -whole-module-optimization --package-path PrimeSwift_8bitBool

FROM swift:6.3.3-slim
WORKDIR /opt/app
COPY --from=build /opt/app/PrimeSwift_1bit_u8/.build/release/PrimeSieveSwift ./PrimeSwift_1bit_u8/.build/release/PrimeSieveSwift
COPY --from=build /opt/app/PrimeSwift_1bitStriped_u8/.build/release/PrimeSieveSwift ./PrimeSwift_1bitStriped_u8/.build/release/PrimeSieveSwift
COPY --from=build /opt/app/PrimeSwift_8bitBool/.build/release/PrimeSieveSwift ./PrimeSwift_8bitBool/.build/release/PrimeSieveSwift
COPY run.sh ./run.sh

ENTRYPOINT [ "./run.sh" ]
