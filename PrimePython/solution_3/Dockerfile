FROM python:3.11-slim

RUN pip install --no-cache-dir numpy numba

WORKDIR /opt/app

COPY PrimePY.py PrimePY_numba.py run.sh ./
RUN chmod +x run.sh

ENTRYPOINT [ "./run.sh" ]
