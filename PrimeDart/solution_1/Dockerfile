FROM dart:3.0.0 AS build
WORKDIR /app
COPY pubspec.* ./
RUN dart pub get
COPY . .
RUN dart pub get --offline \
    && dart compile exe /app/bin/runner.dart -o runner 

FROM scratch AS runtime
WORKDIR /app
COPY --from=build /runtime/ /
COPY --from=build /app/runner ./

ENTRYPOINT [ "./runner" ]
