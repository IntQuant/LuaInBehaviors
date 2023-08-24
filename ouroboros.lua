Ouroboros = {}

function Ouroboros.new_parser(dump)
    local parser = { dump = dump, pointer = 1, info = {}}
    function parser:skip_bytes(count)
        self.pointer = self.pointer + count
    end
    function parser:next_byte()
        local ret = self.dump:byte(self.pointer)
        self.pointer = self.pointer + 1
        return ret
    end
    function parser:next_fmt(fmt)
        local val
        val, self.pointer = string.unpack(fmt, self.dump, self.pointer)
        return val
    end
    function parser:next_int()
        return parser:next_fmt("j")
    end
    function parser:next_num()
        return parser:next_fmt("n")
    end
    return parser
end

function Ouroboros.parse(dump)
    local parser = Ouroboros.new_parser(dump)
    local info = parser.info

    -- Parse header
    parser:skip_bytes(4) -- SIGNATURE
    parser:skip_bytes(1) -- VERSION
    parser:skip_bytes(1) -- FORMAT
    parser:skip_bytes(6) -- DATA
    assert(parser:next_byte() == 4) -- instruction size
    info.ins_size = 4
    info.int_size = parser:next_byte() -- int size
    info.num_size = parser:next_byte() -- number size
    assert(parser:next_int() == 0x5678)
    assert(parser:next_num() == 370.5)

    -- 
end

Ouroboros.parse(string.dump(load("return 1+2+3")))