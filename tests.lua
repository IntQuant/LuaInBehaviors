require("minmus")

local test_id = 1

function test(code, eres)
    print("======================")
    print("Running test", test_id)
    print("======================")
    test_id = test_id + 1
    local res = Minmus.compile_and_run(code)
    assert(res[1].val == eres)
    print()
    print()
end

test([[
    local a = 0
    for i = 1, 3 do
        a = a + i
    end
    for i = 1, 0 do
        a = a + i
    end
    return a
]], 6)

test([[
    local a = 5
    b = 3
    function lalala()
        return a + b
    end
    return lalala()
]], 8)


test([[
    function get_b()
        return 2
    end

    local a = 1
    local b = get_b()
    local c = a + b
    return c
]], 3)

test([[
    local a = 5
    function la()
        a = 6
    end
    la()
    return a
]], 6)

