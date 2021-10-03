-- some primitive functions are implemented in Lua

function pairs(t)
    return next, t, nil
end

function iter (a, i)
    i = i + 1
    local v = a[i]
    if v then
        return i, v
    end
end

function ipairs (a)
    return iter, a, 0
end

function select(i, ...)
    local arg = {...}
    return arg[i]
end

-- standard library for rua
std = {}

function std.instance(object, class)
    
end

function std.subclass(subclass, super)
    
end

function std.super(object)
    return getmetatable
end

function std.class(...)
    local args = {...}
    local n_args = #args
    local def = args[n_args]
    local mt = {}
    mt.__index = mt
    mt['__classname'] = def['__classname']

    local methods = {}

    for methodname,method in next, def do
        if type(method) == 'function' then
            if methodname ~= 'constructor' then
                mt[methodname] = method
                
            end
        end        
    end
    
    
    local ctor = function (...)
        local object = {}
        setmetatable(object, mt)
        def.constructor(object, ...)
        return object
    end
    local classdef = {
        __call = ctor,
    }
    local c = {}
    setmetatable(c, classdef)
    return c
end

