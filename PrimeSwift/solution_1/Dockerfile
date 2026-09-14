FROM swift:6.3.3 AS build

WORKDIR /opt/app

COPY . .

# On x86-64 the striped package targets Haswell (AVX2), which every current x86-64
# machine supports; on arm64 the baseline already covers the vector unit.
RUN swift build -v --configuration release -Xswiftc -O -Xswiftc -whole-module-optimization --package-path PrimeSwift_1bit_u8 && \
    if [ "$(uname -m)" = x86_64 ]; then set -- -Xswiftc -target-cpu -Xswiftc haswell; fi && \
    swift build -v --configuration release -Xswiftc -O -Xswiftc -whole-module-optimization "$@" --package-path PrimeSwift_1bitStriped_u8 && \
    swift build -v --configuration release -Xswiftc -O -Xswiftc -whole-module-optimization --package-path PrimeSwift_8bitBool

FROM swift:6.3.3-slim
WORKDIR /opt/app
COPY --from=build /opt/app/PrimeSwift_1bit_u8/.build/release/PrimeSieveSwift ./PrimeSwift_1bit_u8/.build/release/PrimeSieveSwift
COPY --from=build /opt/app/PrimeSwift_1bitStriped_u8/.build/release/PrimeSieveSwift ./PrimeSwift_1bitStriped_u8/.build/release/PrimeSieveSwift
COPY --from=build /opt/app/PrimeSwift_8bitBool/.build/release/PrimeSieveSwift ./PrimeSwift_8bitBool/.build/release/PrimeSieveSwift
COPY run.sh ./run.sh

ENTRYPOINT [ "./run.sh" ]
