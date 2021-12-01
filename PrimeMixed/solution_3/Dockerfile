FROM python:3-alpine

RUN apk add build-base --no-cache && \
    /usr/sbin/adduser -u 1001 -D michaelgrn

# Set the venv location
ENV VIRTUAL_ENV=/opt/venv
# Install venv in the specified location
RUN python3 -m venv $VIRTUAL_ENV
# Add the Python from the venv location to the PATH so we can use it
ENV PATH="$VIRTUAL_ENV/bin:$PATH"

WORKDIR /home/michaelgrn
COPY setup.py .
COPY src/ ./src/

RUN pip install --upgrade pip -e . --no-cache-dir

USER michaelgrn

CMD ["python", "-m", "pysieve"]
