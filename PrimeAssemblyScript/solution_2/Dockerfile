FROM node:16

WORKDIR /usr/src/app
COPY package*.json ./

RUN npm install
COPY . .


RUN npm run build:orig
RUN npm run build:unrolled

CMD [ "npm", "run", "bench:orig" ]
CMD [ "npm", "run", "bench:unrolled" ]
