FROM node:14

WORKDIR /usr/src/app
COPY package*.json ./

RUN npm install
COPY . .


RUN npm run build:bench

CMD [ "npm", "run", "bench" ]
