FROM fedora:34

# install prerequisites and build Nim from source...
RUN dnf --refresh -y upgrade && dnf install -y wget xz gcc \
      && mkdir nim-lang && cd nim-lang \
      && wget https://nim-lang.org/download/nim-1.4.8.tar.xz \
      && tar -axf nim-1.4.8.tar.xz && cd nim-1.4.8 \
      && sh build.sh && bin/nim c -d:release koch && ./koch boot -d:release \
      && cd .. && rm -rdf nim-1.4.8.tar.xz && cd ..

# compile program...
WORKDIR /opt/app
COPY primes.nim .
RUN PATH=/nim-lang/nim-1.4.8/bin:$PATH nim c --gc:arc -d:danger -t:-march=native -d:lto primes.nim

# program binary ready to run...
ENTRYPOINT [ "/opt/app/primes" ]
