FROM node:26-alpine3.24

RUN npm install -g pyret-npm@0.0.27

WORKDIR /opt/app
COPY primes*.arr ./
RUN pyret -qkc primes.arr
ENTRYPOINT ["node", "primes.jarr"]
