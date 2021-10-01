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