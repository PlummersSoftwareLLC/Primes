FROM debian:10

RUN apt-get update && apt-get install -y --no-install-recommends \
    wget \
    ca-certificates \
    curl \
    gcc \
    g++ \
    perl \
    python3.7 \
    python3-distutils \
    python3-venv \
    python \
    python-dev \
    python-setuptools \
    libgmp10 \
    libgmp-dev \
    locales \
    bash \
    make \
    mawk \
    file \
    pkg-config \
    git \
    && rm -rf /var/lib/apt/lists/*

RUN update-alternatives --install /usr/bin/python python /usr/bin/python3.7 20
RUN update-alternatives --set python /usr/bin/python3.7

ENV CHPL_VERSION 1.24.1
ENV CHPL_HOME    /opt/chapel/$CHPL_VERSION
ENV CHPL_GMP     system

RUN mkdir -p /opt/chapel \
    && wget -q -O - https://github.com/chapel-lang/chapel/releases/download/$CHPL_VERSION/chapel-$CHPL_VERSION.tar.gz | tar -xzC /opt/chapel --transform 's/chapel-//' \
    && make -C $CHPL_HOME \
    && make -C $CHPL_HOME chpldoc test-venv mason \
    && make -C $CHPL_HOME cleanall

# Configure locale
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    echo 'LANG="en_US.UTF-8"'>/etc/default/locale && \
    dpkg-reconfigure --frontend=noninteractive locales && \
    update-locale LANG=en_US.UTF-8

# Configure dummy git user
RUN git config --global user.email "noreply@example.com" && \
    git config --global user.name  "Chapel user"

ENV PATH $PATH:$CHPL_HOME/bin/linux64-x86_64:$CHPL_HOME/util

WORKDIR /opt/app
COPY primes.chpl .
RUN chpl --fast primes.chpl

ENTRYPOINT [ "/opt/app/primes" ]

# To run with Docker commands...
# docker build -t primes .
# docker run --rm primes

# To run from a local folder mapped to the Docker image...
# to build:
# docker run --rm -v "$PWD":/usr/src/myapp -w /usr/src/myapp chapel/chapel chpl --fast -o primes primes.chpl
# To run:
# docker run --rm -v "$PWD":/usr/src/myapp -w /usr/src/myapp chapel/chapel ./hello
