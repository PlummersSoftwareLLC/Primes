FROM dart:2.15.1 AS build
WORKDIR /app
COPY pubspec.* ./
RUN pub get
COPY . .
RUN pub get --offline \
    && dart compile exe /app/bin/Runner.dart -o Runner 

FROM scratch AS runtime
WORKDIR /app
COPY --from=build /runtime/ /
COPY --from=build /app/Runner ./

ENTRYPOINT [ "./Runner" ]
