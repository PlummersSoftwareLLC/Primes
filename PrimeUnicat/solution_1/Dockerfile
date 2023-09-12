FROM python:3.11.5-alpine3.18

WORKDIR /opt/app
COPY *.cat *.py ./
RUN pip install --no-cache-dir unicat-esolang==0.1.4

ENTRYPOINT ["./run_primes.py"]
