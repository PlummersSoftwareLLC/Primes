FROM python:3.14-alpine3.24

WORKDIR /opt/app
COPY *.cat *.py ./
RUN pip install --no-cache-dir unicat-esolang==0.1.4

ENTRYPOINT ["./run_primes.py"]
