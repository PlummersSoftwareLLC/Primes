--  A direct method of benchmarking a single generated script

local ARGS = {...}
local ffi = require "ffi"

local name = ARGS[1]
local type = ARGS[2] or "ffi"
local size = ARGS[3] or 1000000
local opt = ARGS[4] or 24000

local buf = {}

if (type == "ffi") then
    buf = ffi.new("uint8_t[?]", size + 999)
end

local chunk, err = loadfile ("./compiled/"..name..".lua")
print(chunk, err)
print(chunk()(buf, 5, opt))