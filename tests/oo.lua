function class(def)
    local ctor = function (...)
        local object = {}
        for methodname,method in next, def do
            local method = method
            if methodname ~= 'new' then
                print(object)
                object[methodname] = function (...)
                    print(object)
                    local t = method(object, ...)
                    -- print(object)
                    return t
                end
            end
        end
        
        def.new(object, ...)
        object.print()
        return object
    end
    -- local classdef = {
    --     __call = ctor,
    -- }
    -- local c = {}
    -- setmetatable(c, classdef)
    -- return c
    return ctor
end

Point = class{
    new=function (self, x,y)
        self.x = x
        self.y = y
    end,
    print = function (self)
        print(self.x, self.y)
    end
}
p = Point(1,2)
p.print()