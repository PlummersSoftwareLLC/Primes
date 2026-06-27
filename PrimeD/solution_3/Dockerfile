FROM vreitech/docker-ldc:2-v1.42.0

WORKDIR /opt/app
COPY . .
RUN dub build --compiler=ldc2 --build=release

ENTRYPOINT [ "/opt/app/prime-sieve-d" ]
