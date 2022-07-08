# use the image below as the preffered base image
# see CONTRIBUTING.md
FROM jarredsumner/bun:edge

WORKDIR /opt/app

COPY PrimeBun.js PrimeBun_memcopy.js run.sh ./

ENTRYPOINT [ "./run.sh" ]
