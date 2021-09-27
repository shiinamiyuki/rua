-- require('tests/unittest.lua')

-- function bind(g, a)
--     local t = g
--     return function (...)
--         return t(a, ...)
--     end
-- end
-- function f(x,y)
--     return x+y
-- end
-- ut = unittest.new('closure')
-- ut:add('bind', function ()
--     assert(bind(f,2)(3) ==5)
-- end)
-- ut:run()

function mystery(f)
    local ctor = function ()
        local object = {}
        
        object.f = function()
            f(object)
        end
        -- object.f()
        -- print('inside end')
        return object
    end
    return ctor
end


local o = mystery(print)({})
print(o)
o.f()
function dummy()
end

function foo(f)
    return function()
        local o = {}
        o.f= function()
            return f(o)
        end
        -- print('g')
        dummy()
        return o
    end
end

-- foo(print)().f()
local o = foo(print)()
o.f()