# use the image below as the preffered base image
# see CONTRIBUTING.md
FROM node:current-alpine3.13

WORKDIR /opt/app

COPY PrimeNode.js .

ENTRYPOINT [ "node", "PrimeNode.js" ]