FROM node:14-alpine AS build

WORKDIR /opt/app

COPY package*.json ./
RUN npm ci

COPY tsconfig.json .
COPY src ./src
RUN npm run build
RUN npm ci --production

FROM node:14-alpine

WORKDIR /opt/app

COPY --from=build /opt/app/package*.json ./
COPY --from=build /opt/app/node_modules ./node_modules
COPY --from=build /opt/app/build ./build

ENTRYPOINT [ "npm", "start" ]
