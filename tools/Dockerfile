FROM alpine:3.13

RUN apk add --no-cache build-base bash \
    python3 python3-dev \
    py3-pip py3-click py3-numpy py3-pandas

WORKDIR /opt/app

# COPY requirements.txt .
# RUN pip3 install -r requirements.txt

COPY tools/ tools/

ENTRYPOINT [ "python3", "/opt/app/tools/cli.py" ]
