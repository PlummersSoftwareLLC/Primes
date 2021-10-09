FROM python:3-alpine

RUN apk add build-base --no-cache \
    && /usr/sbin/adduser -u 1001 -D michaelgrn

WORKDIR /home/michaelgrn

RUN python -m venv venv \
    && chown -R michaelgrn /home/michaelgrn

USER michaelgrn

ENV HOME=/home/michaelgrn
ENV PATH=$HOME/.local/bin:$PATH

COPY --chown=michaelgrn setup.py .
COPY --chown=michaelgrn src/ ./src/

#hadolint ignore=SC1091
RUN . venv/bin/activate \
    && python -m pip install -e . --user --no-cache-dir

CMD ["python", "-m", "pysieve"]
