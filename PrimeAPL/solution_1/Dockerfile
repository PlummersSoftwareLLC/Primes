FROM dyalog/dyalog:18.0

WORKDIR /app

COPY PrimeSieveAPL.apln .

ENV LOAD=/app/PrimeSieveAPL.apln
ENTRYPOINT ["dyalog", "-b", "-s", "2>/dev/null"]
