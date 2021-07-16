FROM python:3.8-alpine3.13

RUN apk add --update --no-cache py3-numpy
ENV PYTHONPATH=/usr/lib/python3.8/site-packages

WORKDIR /opt/app

COPY PrimePY.py .

ENTRYPOINT [ "python3", "PrimePY.py" ]