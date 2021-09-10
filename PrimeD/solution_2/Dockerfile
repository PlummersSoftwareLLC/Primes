FROM alpine:3.13
WORKDIR /app
COPY . .
RUN apk update \ 
    && apk add curl=7.78.0-r0 bash=5.1.0-r0 xz=5.2.5-r0 build-base=0.5-r2 ldc=1.24.0-r0 dub=1.23.0-r0 libgcc=10.2.1_pre1-r3 --no-cache \
    && dub build -b release --compiler=ldc2
ENTRYPOINT [ "./solution_2", "-m=leaderboard" ]