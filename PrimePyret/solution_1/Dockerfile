FROM rzuckerm/pyret:0.0.27-3

WORKDIR /opt/app
COPY primes*.arr ./
RUN pyret -qkc primes.arr
ENTRYPOINT ["node", "primes.jarr"]
