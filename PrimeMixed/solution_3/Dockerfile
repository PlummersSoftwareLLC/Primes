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

# The following is ignored, due to way venv is used. If you're a Python developer with knowledge of Docker. venv and user installs consider giving your insight here: https://github.com/PlummersSoftwareLLC/Primes/pull/744
# hadolint ignore=SC1091
RUN . venv/bin/activate
# hadolint ignore=DL3059
RUN python -m pip install -e . --user --no-cache-dir

CMD ["python", "-m", "pysieve"]
