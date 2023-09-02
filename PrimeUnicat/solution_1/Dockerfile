FROM python:3.11.5-alpine3.18

WORKDIR /opt/app
COPY *.cat *.py ./
RUN pip install unicat-esolang

ENTRYPOINT ["./run_primes.py"]
