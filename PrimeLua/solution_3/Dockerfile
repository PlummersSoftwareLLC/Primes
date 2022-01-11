FROM nickblah/luajit:2.1-beta-alpine

WORKDIR /prog
COPY prog.lua .

ENTRYPOINT [ "luajit", "prog.lua", "b" ]
