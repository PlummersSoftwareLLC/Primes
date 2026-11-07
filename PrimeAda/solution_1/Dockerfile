FROM alpine:3.24 AS build

RUN apk add --no-cache gcc-gnat libc-dev

WORKDIR /opt/app

COPY main.adb .

RUN gnatmake -gnatwa -gnatn -gnatp \
    -Ofast \
    -funroll-all-loops \
    -march=native \
    -mtune=native \
    main.adb \
    -bargs -static \
    -largs -static

FROM alpine:3.24

COPY --from=build /opt/app/main /main

ENTRYPOINT ["/main"]