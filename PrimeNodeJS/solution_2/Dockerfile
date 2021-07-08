# use the image below as the preffered base image
# see CONTRIBUTING.md
FROM node:current-alpine3.13

WORKDIR /opt/app

COPY *.js .

ENTRYPOINT [ "node", "PrimeNode_cluster.js" ]
