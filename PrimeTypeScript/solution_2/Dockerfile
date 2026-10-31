FROM node:26-alpine3.24 AS build

WORKDIR /opt/app

COPY . .
RUN npm ci \
    && npm run build \
    && npm ci --omit=dev

FROM node:26-alpine3.24

WORKDIR /opt/app

COPY --from=build /opt/app/package*.json ./
COPY --from=build /opt/app/node_modules ./node_modules
COPY --from=build /opt/app/build ./build

ENTRYPOINT [ "npm", "start" ]