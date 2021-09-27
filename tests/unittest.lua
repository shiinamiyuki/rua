unittest = {}
unittest.__index = unittest

function unittest.new(name)
    local ut = {}
    ut.testcase ={}
    setmetatable(ut, unittest)
    return ut
end

function unittest:add(name,testcase)
    print(self, name, testcase)
    assert(type(testcase) == 'function')
    self.testcase[name] = testcase
end

function g()
    return 1,2
end
function unittest:run()
    for n, t in next, self.testcase do
        print('Running testcase', n)
        t()
    end
end