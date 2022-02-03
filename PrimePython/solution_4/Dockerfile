FROM python:3-slim

WORKDIR /opt/app

# Set the venv location
ENV VIRTUAL_ENV=/opt/venv
# Install venv in the specified location
RUN python3 -m venv $VIRTUAL_ENV
# Add the Python from the venv location to the PATH so we can use it
ENV PATH="$VIRTUAL_ENV/bin:$PATH"

RUN pip install numba --upgrade pip --no-cache-dir

COPY PrimePY.py .

ENTRYPOINT [ "python3", "PrimePY.py" ]
