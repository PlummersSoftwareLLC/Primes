FROM google/dart
WORKDIR /app
ADD pubspec.* /app/
RUN pub get
ADD . /app
RUN pub get --offline
RUN dart2native /app/bin/PrimeDart.dart -o /app/bin/PrimeDart


FROM subfuzion/dart-scratch
COPY --from=0 /app/bin/PrimeDart /app/bin/PrimeDart
ENTRYPOINT [ "/app/bin/PrimeDart" ]