local n = 10000000
local sum = 0.0
local i=0
local dx = 1.0/n
local x=0.0
while i<n do
    sum = sum + 1.0/(x*x+1.0)
    x = x+ dx
    i = i+ 1
end
print(sum * 4.0 / n)