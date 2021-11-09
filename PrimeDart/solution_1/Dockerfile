FROM google/dart:2.14.0 AS build
WORKDIR /app
COPY pubspec.* ./
RUN pub get
COPY . .
RUN pub get --offline \
    && dart compile exe /app/bin/Runner.dart -o Runner \
    && dart compile exe /app/bin/PrimeDart.dart -o PrimeDart \
    && dart compile exe /app/bin/PrimeDartOneBit.dart -o PrimeDartOneBit \
    && dart compile exe /app/bin/PrimeDartParallel.dart -o PrimeDartParallel \
    && dart compile exe /app/bin/PrimeDartParallelOneBit.dart -o PrimeDartParallelOneBit

FROM ubuntu:20.04 AS runtime
WORKDIR /app
COPY --from=build /app/Runner /app/PrimeDart* ./

ENTRYPOINT [ "./Runner", "./PrimeDart", "./PrimeDartParallel", "./PrimeDartOneBit", "./PrimeDartParallelOneBit" ]
