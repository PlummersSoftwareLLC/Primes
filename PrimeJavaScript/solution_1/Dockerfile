# use the image below as the preffered base image
# see CONTRIBUTING.md
FROM diefenbakerbell/ubuntu-bun:0.1.2

WORKDIR /opt/app

SHELL [ "/bin/bash", "-o", "pipefail", "-c" ]

RUN apt-get update &&\
    apt-get install --no-install-recommends -y \
        curl=7.81.0-1ubuntu1.3 \
        zip=3.0-12build2 \
        unzip=6.0-26ubuntu3 \
    &&\
    # NodeJS
    curl -fsSL https://deb.nodesource.com/setup_18.x | bash - &&\
    apt-get install --no-install-recommends -y \
        nodejs=18.4.0-deb-1nodesource1 \
    &&\
    # Deno
    curl -fsSL https://deno.land/x/install/install.sh | sh -s v1.23.3 &&\
    ln -s /root/.deno/bin/deno /usr/bin &&\
    # cleanup
    apt-get remove -y curl zip unzip &&\
    apt-get autoclean &&\
    rm -r /var/lib/apt/lists/*

COPY run.sh \
     PrimeJavaScript.js \
     PrimeJavaScript_memcopy.js \
     PrimeJavaScript_cluster.js \
     PrimeJavaScript_worker_main.mjs \
     PrimeJavaScript_worker_child.mjs \
     PrimeSieve.mjs \
    ./

# RUN echo "" && \
#     echo "Node version: $(node --version)" && \
#     echo "Bun version: $(bun --version)" && \
#     echo "Deno version: $(deno --version)" && \
#     echo ""

ENTRYPOINT [ "./run.sh" ]

# For manual debugging
# ENTRYPOINT [ "/bin/bash" ]
