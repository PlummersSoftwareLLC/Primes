FROM dart:2.15.1 AS build
WORKDIR /app
COPY pubspec.* ./
RUN pub get
COPY . .
RUN pub get --offline \
    && dart compile exe /app/bin/Runner.dart -o Runner \
    && dart compile exe /app/bin/PrimeDart.dart -o PrimeDart \
    && dart compile exe /app/bin/PrimeDartOneBit.dart -o PrimeDartOneBit \
    && dart compile exe /app/bin/PrimeDartParallel.dart -o PrimeDartParallel \
    && dart compile exe /app/bin/PrimeDartParallelOneBit.dart -o PrimeDartParallelOneBit

FROM scratch AS runtime
WORKDIR /app
COPY --from=build /runtime/ /
COPY --from=build /app/Runner /app/PrimeDart* ./

# Parallel implementations were removed because they cause a hang on the 5950X runner that's in use on the project. 
# They can be reinstated once that issue has been resolved. Original line was:
# ENTRYPOINT [ "./Runner", "./PrimeDart", "./PrimeDartParallel", "./PrimeDartOneBit", "./PrimeDartParallelOneBit" ]
ENTRYPOINT [ "./Runner", "./PrimeDart", "./PrimeDartOneBit" ]
