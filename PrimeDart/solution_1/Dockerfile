FROM google/dart:2.14.0 AS build
WORKDIR /app
COPY pubspec.* /app/
RUN pub get
COPY . /app
RUN pub get --offline
RUN dart compile exe /app/bin/Runner.dart -o Runner
RUN dart compile exe /app/bin/PrimeDart.dart -o PrimeDart
RUN dart compile exe /app/bin/PrimeDartOneBit.dart -o PrimeDartOneBit
RUN dart compile exe /app/bin/PrimeDartParallel.dart -o PrimeDartParallel
RUN dart compile exe /app/bin/PrimeDartParallelOneBit.dart -o PrimeDartParallelOneBit

FROM ubuntu:20.04 AS runtime
WORKDIR /app
COPY --from=build /app/Runner /app/PrimeDart* /app/

ENTRYPOINT [ "./Runner", "./PrimeDart", "./PrimeDartParallel", "./PrimeDartOneBit", "./PrimeDartParallelOneBit" ]
