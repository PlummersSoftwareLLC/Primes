FROM node:16

WORKDIR /usr/src/app
COPY package*.json ./

RUN npm install
COPY . .


RUN npm run build:all

CMD [ "npm", "run", "bench:all" ]
