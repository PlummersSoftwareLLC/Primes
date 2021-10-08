FROM python:3-alpine

RUN apk add build-base --no-cache && \
    /usr/sbin/adduser -u 1001 -D michaelgrn

ENV VIRTUAL_ENV=/opt/venv
RUN python3 -m venv $VIRTUAL_ENV
ENV PATH="$VIRTUAL_ENV/bin:$PATH"

COPY --chown=michaelgrn setup.py .
COPY --chown=michaelgrn src/ ./src/

RUN python -m pip install --upgrade pip && \
    pip install -e . --no-cache-dir

USER michaelgrn

WORKDIR /home/michaelgrn

CMD ["python", "-m", "pysieve"]