FROM alpine:3.24
WORKDIR /app
COPY . .
RUN apk update \ 
    && apk add build-base ldc dub libgcc zlib-dev --no-cache \
    && dub build -b release --compiler=ldc2
ENTRYPOINT [ "./solution_2", "-m=leaderboard" ]