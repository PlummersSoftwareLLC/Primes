FROM google/dart
WORKDIR /app
ADD pubspec.* /app/
RUN pub get
ADD . /app
RUN pub get --offline
RUN dart compile exe /app/bin/Runner.dart -o /Runner
RUN dart compile exe /app/bin/PrimeDart.dart -o /PrimeDart
RUN dart compile exe /app/bin/PrimeDartOneBit.dart -o /PrimeDartOneBit
RUN dart compile exe /app/bin/PrimeDartParallel.dart -o /PrimeDartParallel
RUN dart compile exe /app/bin/PrimeDartParallelOneBit.dart -o /PrimeDartParallelOneBit

ENTRYPOINT [ "/Runner", "/PrimeDart", "/PrimeDartParallel", "/PrimeDartOneBit", "/PrimeDartParallelOneBit" ]
