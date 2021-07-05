FROM alpine

RUN apk add --no-cache gcc-gnat libc-dev

COPY main.adb .

RUN gnatmake -gnatwa -gnatn -gnatp \
    -Ofast \
    -funroll-all-loops \
    -march=native \
    -mtune=native \
    main.adb

ENTRYPOINT ["/main"]
