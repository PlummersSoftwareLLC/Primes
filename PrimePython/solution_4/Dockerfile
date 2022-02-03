FROM python:3alpine3

WORKDIR /opt/app

RUN pip install --upgrade pip -e numba --no-cache-dir

COPY PrimePY.py .

ENTRYPOINT [ "python3", "PrimePY.py" ]
