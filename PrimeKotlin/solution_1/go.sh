docker build --pull --rm -f Dockerfile -t kotlin:latest .
docker run --rm -it -v $PWD:/opt/app kotlin:latest
