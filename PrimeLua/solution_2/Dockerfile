FROM nickblah/luajit:2.1-beta-alpine

WORKDIR /app

COPY PrimeLua.lua .

ENTRYPOINT [ "luajit", "PrimeLua.lua" ]
