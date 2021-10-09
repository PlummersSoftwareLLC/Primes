FROM node:14-alpine AS build

WORKDIR /opt/app

COPY . .
RUN npm ci \
    && npm run build \
    && npm ci --production

FROM node:14-alpine

WORKDIR /opt/app

COPY --from=build /opt/app/package*.json ./
COPY --from=build /opt/app/node_modules ./node_modules
COPY --from=build /opt/app/build ./build

ENTRYPOINT [ "npm", "start" ]