FROM google/dart:2.13.4
WORKDIR /app
COPY pubspec.* /app/
RUN pub get
COPY . /app
RUN pub get --offline
RUN dart2native /app/bin/PrimeDart.dart -o /app/bin/PrimeDart


FROM ubuntu:20.04
COPY --from=0 /app/bin/PrimeDart /app/bin/PrimeDart
ENTRYPOINT [ "/app/bin/PrimeDart" ]