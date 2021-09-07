FROM alpine:3.13 AS build

RUN apk add --no-cache gcc-gnat=10.2.1_pre1-r3 libc-dev=0.7.2-r3

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

FROM alpine:3.13

COPY --from=build /opt/app/main /main

ENTRYPOINT ["/main"]