FROM dart:2.15.1 AS build
WORKDIR /app
COPY pubspec.* ./
RUN pub get
COPY . .
RUN pub get --offline \
    && dart compile exe /app/bin/runner.dart -o runner 

FROM scratch AS runtime
WORKDIR /app
COPY --from=build /runtime/ /
COPY --from=build /app/runner ./

ENTRYPOINT [ "./runner" ]
