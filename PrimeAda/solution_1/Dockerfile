FROM alpine:3.13 AS build

RUN apk add --no-cache gcc-gnat libc-dev

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

COPY --from=build /main /main

ENTRYPOINT ["/main"]
