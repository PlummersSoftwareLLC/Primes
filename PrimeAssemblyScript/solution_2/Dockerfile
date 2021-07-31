FROM node:16.6.0-alpine3.14

WORKDIR /usr/src/app
COPY package*.json ./

RUN npm install
COPY . .


RUN npm run build:bench

CMD [ "npm", "run", "bench" ]
