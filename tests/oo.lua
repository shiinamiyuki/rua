function Class(def)
    local mt = {}
    mt.__index = mt
    for methodname,method in next, def do
        if methodname ~= 'new' then
            mt[methodname] = method
        end
    end
    
    
    local ctor = function (...)
        local object = {}
        -- for methodname,method in next, def do
        --     if methodname ~= 'new' then
        --         object[methodname] = method
        --     end
        -- end
        setmetatable(object, mt)
        def.new(object, ...)
        return object
    end
    local classdef = {
        __call = ctor,
    }
    local c = {}
    setmetatable(c, classdef)
    return c
end

Point = Class{
    new=function (self, x,y)
        self.x = x
        self.y = y
    end,
    print = function (self)
        print(self.x, self.y)
    end,
    __add = function (self, rhs)
        return Point(self.x + rhs.x, self.y+rhs.y)
    end,
    __index = function (self, i)
        if i == 1 then
            return self.x
        elseif i == 2 then
            return self.y
        else
            return rawget(getmetatable(self), i)
        end
    end
}
local p = Point(1,2)
p:print()
local q = p + p
q:print()
print(getmetatable(p) == getmetatable(q))
print(p[1], p[2], p.__index)
