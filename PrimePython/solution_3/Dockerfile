FROM python:3.14-slim-trixie

RUN pip install --no-cache-dir numpy==2.5.4 numba==0.68.1

WORKDIR /opt/app

COPY PrimePY.py PrimePY_numba.py run.sh ./
RUN chmod +x run.sh

ENTRYPOINT [ "./run.sh" ]
