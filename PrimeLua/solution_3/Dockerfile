FROM nickblah/luajit:2.1-beta-alpine

WORKDIR /prog
COPY prog.lua .
COPY l1_sieve.lua .
COPY compiled/ ./compiled

ENTRYPOINT [ "luajit", "prog.lua", "b" ]
