FROM dyalog/dyalog:18.0

WORKDIR /opt/app

COPY PrimeSieveAPL.apln .
COPY bench.sh .

ENTRYPOINT ["./bench.sh"]
