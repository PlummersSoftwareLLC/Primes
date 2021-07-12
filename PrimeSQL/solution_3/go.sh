docker build --pull --rm -f Dockerfile -t oracle:latest .
docker run --rm -it -v $PWD:/opt/app oracle:latest
