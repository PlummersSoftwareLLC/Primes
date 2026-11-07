FROM dyalog/dyalog:20.0

WORKDIR /app

COPY PrimeSieveAPL.apln .

ENV LOAD=/app/PrimeSieveAPL.apln
ENTRYPOINT ["dyalog", "-b", "-s", "2>/dev/null"]
