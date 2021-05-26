#
# To conform to the drag race standard using the official node
#FROM node:current-alpine3.13

FROM node:latest

WORKDIR /opt/app

COPY PrimeNode.js .

ENTRYPOINT [ "node", "PrimeNode.js" ]