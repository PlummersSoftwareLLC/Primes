FROM gcc:9.4.0 as c
WORKDIR /app
COPY /c_library .
RUN gcc -c -Ofast c_sieve.c \
    && gcc -shared -o c_sieve.so c_sieve.o

FROM dart:2.15.1 AS dart
WORKDIR /app
COPY /bin .
RUN dart compile exe runner.dart -o runner 

FROM scratch AS runtime
WORKDIR /app
COPY --from=c /app/c_sieve.so ./
COPY --from=dart /runtime/ /
COPY --from=dart /app/runner ./

ENTRYPOINT [ "./runner" ]
