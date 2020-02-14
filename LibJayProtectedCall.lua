local MAJOR = "LibJayProtectedCall"
local MINOR = 1

assert(LibStub, format("%s requires LibStub.", MAJOR))

local lib = LibStub:NewLibrary(MAJOR, MINOR)

if not lib then return end

local safecall, xsafecall
do -- safecall, xsafecall
    local pcall = pcall
    ---@param func function
    ---@return boolean retOK
    safecall = function(func, ...) return pcall(func, ...) end

    local geterrorhandler = geterrorhandler
    ---@param err string
    ---@return function handler
    local function errorhandler(err) return geterrorhandler()(err) end

    local xpcall = xpcall
    ---@param func function
    ---@return boolean retOK
    xsafecall = function(func, ...) return xpcall(func, errorhandler, ...) end
end

local tnew, tdel
do -- tnew, tdel

    local cache = setmetatable({}, {__mode = "k"})

    local next = next
    local select = select
    ---@return table t
    function tnew(...)
        local t = next(cache)
        if t then
            cache[t] = nil
            local n = select("#", ...)
            for i = 1, n do t[i] = select(i, ...) end
            return t
        end
        return {...}
    end

    local wipe = wipe
    ---@param t table
    function tdel(t) cache[wipe(t)] = true end
end

local packargs
do -- pack2
    local select = select
    ---@return table args
    function packargs(...) return {n = select("#", ...), ...} end
end

local unpackargs
do -- pack2
    local unpack = unpack
    ---@param t table
    function unpackargs(t) return unpack(t, 1, t.n) end
end

local getKey
do -- getKey
    local strhash
    do -- strhash
        local fmod = math.fmod
        local strbyte = strbyte
        local strlen = strlen
        ---@param str string
        ---@return string str
        function strhash(str)
            local counter = 1
            local len = strlen(str)
            for i = 1, len, 3 do
                counter =
                    fmod(counter * 8161, 4294967279) + -- 2^32 - 17: Prime!
                        (strbyte(str, i) * 16776193) +
                        ((strbyte(str, i + 1) or (len - i + 256)) * 8372226) +
                        ((strbyte(str, i + 2) or (len - i + 256)) * 3932164)
            end
            return fmod(counter, 4294967291) -- 2^32 - 5: Prime (and different from the prime in the loop)
        end
    end

    local getstring
    do -- getstring
        local tostring = tostring

        local prefixes = setmetatable({}, {
            __index = function(t, k)
                local v = tostring(function() end) .. "%s"
                t[k] = v
                return v
            end
        })

        local format = format
        local type = type
        ---@param str string
        ---@return string str
        function getstring(arg)
            return format(prefixes[type(arg)], tostring(arg))
        end
    end

    local select = select
    local tconcat = table.concat
    ---@return string key
    function getKey(...)
        local keys = tnew()
        for i = 1, select("#", ...) do
            keys[i] = getstring(select(i, ...))
        end
        local key = strhash(tconcat(keys))
        tdel(keys)
        return key
    end
end

lib.registry = lib.registry or {}
local registry = lib.registry

lib.funcs = lib.funcs or {}
local funcs = lib.funcs

local wipe = wipe
local LJEvent = LibStub("LibJayEvent")
local function PLAYER_REGEN_ENABLED()
    LJEvent:Unregister("PLAYER_REGEN_ENABLED", PLAYER_REGEN_ENABLED)
    for i = 1, #registry do funcs[registry[i]]() end
    wipe(funcs)
    wipe(registry)
end

local select = select
---@param x boolean
---@param func function
---@return function regFunc
local function getRegFunc(x, func, ...)
    local safecall = x and xsafecall or safecall
    if select("#", ...) == 0 then
        return function() safecall(func) end
    else
        local args = packargs(...)
        return function() safecall(func, unpackargs(args)) end
    end
end

local InCombatLockdown = InCombatLockdown
---@param func function
function lib:Call(func, ...)
    if InCombatLockdown() then
        local regKey = getKey(func, ...)
        funcs[regKey] = getRegFunc(false, func, ...)
        registry[#registry + 1] = regKey
        LJEvent:Register("PLAYER_REGEN_ENABLED", PLAYER_REGEN_ENABLED)
    else
        safecall(func, ...)
    end
end
setmetatable(lib, {__call = lib.Call})

local tDeleteItem = tDeleteItem
---@param func function
function lib:CallOnce(func, ...)
    if InCombatLockdown() then
        local regKey = getKey(func, ...)
        if funcs[regKey] then
            tDeleteItem(registry, regKey)
        else
            funcs[regKey] = getRegFunc(false, func, ...)
        end
        registry[#registry + 1] = regKey
        LJEvent:Register("PLAYER_REGEN_ENABLED", PLAYER_REGEN_ENABLED)
    else
        safecall(func, ...)
    end
end

local error = error
local format = format
local type = type
---@param unprotectedFunction function
---@return function protectedFunction
function lib:Create(unprotectedFunction)
    if type(unprotectedFunction) ~= "function" then
        error(format(
                  "Usage: %s:Create(unprotectedFunction): 'unprotectedFunction' - function expected got %s",
                  MAJOR, type(unprotectedFunction)), 2)
    end
    return function(...) self:Call(unprotectedFunction, ...) end
end

---@param unprotectedFunction function
---@return function protectedFunction
function lib:CreateOnce(unprotectedFunction)
    if type(unprotectedFunction) ~= "function" then
        error(format(
                  "Usage: %s:CreateOnce(unprotectedFunction): 'unprotectedFunction' - function expected got %s",
                  MAJOR, type(unprotectedFunction)), 2)
    end
    return function(...) self:CallOnce(unprotectedFunction, ...) end
end

---@param tbl table
---@param functionKey any
function lib:Protect(tbl, functionKey)
    if type(tbl) ~= "table" then
        error(format(
                  "Usage: %s:Protect(tbl, functionKey): 'tbl' - table expected got %s",
                  MAJOR, type(tbl)), 2)
    end
    if type(functionKey) == "nil" then
        error(format(
                  "Usage: %s:Protect(tbl, functionKey): 'functionKey' - can't be nil",
                  MAJOR), 2)
    end
    tbl[functionKey] = self:Create(tbl[functionKey])
end

---@param tbl table
---@param functionKey any
function lib:ProtectOnce(tbl, functionKey)
    if type(tbl) ~= "table" then
        error(format(
                  "Usage: %s:ProtectOnce(tbl, functionKey): 'tbl' - table expected got %s",
                  MAJOR, type(tbl)), 2)
    end
    if type(functionKey) == "nil" then
        error(format(
                  "Usage: %s:ProtectOnce(tbl, functionKey): 'functionKey' - can't be nil",
                  MAJOR), 2)
    end
    tbl[functionKey] = self:CreateOnce(tbl[functionKey])
end

---@param func function
function lib:xCall(func, ...)
    if InCombatLockdown() then
        local regKey = getKey(func, ...)
        funcs[regKey] = getRegFunc(true, func, ...)
        registry[#registry + 1] = regKey
        LJEvent:Register("PLAYER_REGEN_ENABLED", PLAYER_REGEN_ENABLED)
    else
        xsafecall(func, ...)
    end
end

---@param func function
function lib:xCallOnce(func, ...)
    if InCombatLockdown() then
        local regKey = getKey(func, ...)
        if funcs[regKey] then
            tDeleteItem(registry, regKey)
        else
            funcs[regKey] = getRegFunc(true, func, ...)
        end
        registry[#registry + 1] = regKey
        LJEvent:Register("PLAYER_REGEN_ENABLED", PLAYER_REGEN_ENABLED)
    else
        xsafecall(func, ...)
    end
end

---@param unprotectedFunction function
---@return function protectedFunction
function lib:xCreate(unprotectedFunction)
    if type(unprotectedFunction) ~= "function" then
        error(format(
                  "Usage: %s:xCreate(unprotectedFunction): 'unprotectedFunction' - function expected got %s",
                  MAJOR, type(unprotectedFunction)), 2)
    end
    return function(...) self:xCall(unprotectedFunction, ...) end
end

---@param unprotectedFunction function
---@return function protectedFunction
function lib:xCreateOnce(unprotectedFunction)
    if type(unprotectedFunction) ~= "function" then
        error(format(
                  "Usage: %s:xCreateOnce(unprotectedFunction): 'unprotectedFunction' - function expected got %s",
                  MAJOR, type(unprotectedFunction)), 2)
    end
    return function(...) self:xCallOnce(unprotectedFunction, ...) end
end

---@param tbl table
---@param functionKey any
function lib:xProtect(tbl, functionKey)
    if type(tbl) ~= "table" then
        error(format(
                  "Usage: %s:xProtect(tbl, functionKey): 'tbl' - table expected got %s",
                  MAJOR, type(tbl)), 2)
    end
    if type(functionKey) == "nil" then
        error(format(
                  "Usage: %s:xProtect(tbl, functionKey): 'functionKey' - can't be nil",
                  MAJOR), 2)
    end
    tbl[functionKey] = self:xCreate(tbl[functionKey])
end

---@param tbl table
---@param functionKey any
function lib:xProtectOnce(tbl, functionKey)
    if type(tbl) ~= "table" then
        error(format(
                  "Usage: %s:xProtectOnce(tbl, functionKey): 'tbl' - table expected got %s",
                  MAJOR, type(tbl)), 2)
    end
    if type(functionKey) == "nil" then
        error(format(
                  "Usage: %s:xProtectOnce(tbl, functionKey): 'functionKey' - can't be nil",
                  MAJOR), 2)
    end
    tbl[functionKey] = self:xCreateOnce(tbl[functionKey])
end
