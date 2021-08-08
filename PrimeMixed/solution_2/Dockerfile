FROM python:3-alpine

USER root

WORKDIR /code/

RUN apk add build-base

ARG CACHEBUST_APP=1

COPY setup.py /code/
COPY src/ /code/src/

RUN pip install -e .

CMD pysieve
