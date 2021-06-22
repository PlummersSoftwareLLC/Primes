docker build --pull --rm -f Dockerfile -t mariadb:latest .
#docker run --rm -it -v $PWD/tmp:/opt/app/ mariadb:latest
docker run --rm -it mariadb:latest
