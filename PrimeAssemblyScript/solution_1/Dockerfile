FROM node:26-trixie-slim

WORKDIR /usr/src/app
COPY package*.json ./

RUN npm install
COPY . .


RUN npm run build:bench

CMD [ "npm", "run", "bench" ]
