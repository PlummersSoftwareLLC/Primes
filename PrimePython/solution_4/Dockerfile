FROM python:3.11-slim

RUN pip install --no-cache-dir numpy numba

WORKDIR /opt/app

COPY PrimePY.py .

ENTRYPOINT [ "python3", "PrimePY.py" ]
