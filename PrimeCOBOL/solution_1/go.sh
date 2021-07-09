docker build --pull --rm -f Dockerfile -t cobol:latest .
docker run --rm -it cobol:latest