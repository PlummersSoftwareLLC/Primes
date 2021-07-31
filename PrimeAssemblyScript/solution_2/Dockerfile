FROM node:16

WORKDIR /usr/src/app
COPY package*.json ./

ENV NODE_ENV production
RUN npm install
COPY . .


RUN npm run build:bench

CMD [ "npm", "run", "bench" ]
