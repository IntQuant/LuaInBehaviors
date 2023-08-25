Minmus = {}

local function makevariant(base, sub)
    return  base | (sub << 4)
end

Minmus.bases = {
    LUA_TNONE = (-1),

    LUA_TNIL = 0,
    LUA_TBOOLEAN = 1,
    LUA_TLIGHTUSERDATA = 2,
    LUA_TNUMBER = 3,
    LUA_TSTRING = 4,
    LUA_TTABLE = 5,
    LUA_TFUNCTION = 6,
    LUA_TUSERDATA = 7,
    LUA_TTHREAD = 8,
}

Minmus.typetags = {
    LUA_VNUMINT = makevariant(Minmus.bases.LUA_TNUMBER, 0),
    LUA_VNUMFLT = makevariant(Minmus.bases.LUA_TNUMBER, 1),

    LUA_VSHRSTR = makevariant(Minmus.bases.LUA_TSTRING, 0),
    LUA_VLNGSTR = makevariant(Minmus.bases.LUA_TSTRING, 1),
}

-- https://stackoverflow.com/questions/9168058/how-to-dump-a-table-to-console
function Minmus.dump(o)
   if type(o) == 'table' then
      local s = '{ '
      for k,v in pairs(o) do
         if type(k) ~= 'number' then k = '"'..k..'"' end
         s = s .. '['..k..'] = ' .. Minmus.dump(v) .. ', '
      end
      return s .. '} '
   else
      return tostring(o)
   end
end

function Minmus.new_parser(dump)
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
    function parser:next_size()
        local x = 0
        repeat
            local b = self:next_byte()
            x = (x << 7) | (b & 0x7f)
        until b & 0x80 > 0
        return x
    end
    function parser:next_string()
        local size = self:next_size()
        if size > 0 then
            size = size - 1
            return self:next_vector(size)
        else
            return nil
        end
    end
    function parser:next_vector(size)
        local str = self.dump:sub(self.pointer, self.pointer+size-1)
        self.pointer = self.pointer + size
        return str
    end
    function parser:next_fn()
        local tags = Minmus.typetags
        local fn = {}
        fn.code_str = self:next_string()
        fn.first_defined = self:next_size()
        fn.last_defined = self:next_size()
        fn.num_params = self:next_byte()
        fn.is_vararg = self:next_byte() > 0
        fn.max_stack_size = self:next_byte()

        -- code
        local code_count = self:next_size()
        print("Instruction count:", code_count)
        local code = self:next_vector(code_count * parser.info.ins_size)
        local intcode = {}
        for i = 1, code_count do
            local ins_start = (i-1) * 4 + 1
            intcode[i] = (code:byte(ins_start + 3) << 24) | (code:byte(ins_start + 2) << 16) | (code:byte(ins_start + 1) << 8) | (code:byte(ins_start + 0) << 0)
        end
        fn.code = intcode
        --print(fn.code:byte(1, code_count * parser.info.ins_size))

        -- constants
        local constant_count = self:next_size()
        print("Constant count:", constant_count)
        local constants = {}
        for i = 1, constant_count do
            local typetag = self:next_byte()
            local const = {
                tag = typetag,
            }
            if typetag == tags.LUA_VNUMINT then
                const.val = self:next_int()
            elseif typetag == tags.LUA_VNUMFLT then
                const.val = self:next_num()
            elseif typetag == tags.LUA_VSHRSTR or typetag == tags.LUA_VLNGSTR then
                const.val = self:next_string()
            end

            constants[i] = const
        end
        fn.constants = constants

        -- upvals
        local upval_count = self:next_size()
        local upvals = {}
        for i = 1, upval_count do
            local upval = {}
            upval.instack = self:next_byte()
            upval.idx = self:next_byte()
            upval.kind = self:next_byte()
            upvals[i] = upval
        end
        fn.upvals = upvals

        -- protos
        local protos_count = self:next_size()
        local protos = {}
        for i = 1, protos_count do
            protos[i] = parser:next_fn()
        end

        fn.protos = protos

        -- debug
        local lineinfo_count = self:next_size()
        fn.lineinfo = self:next_vector(lineinfo_count)
        
        local abs_lineinfo_count = self:next_size()
        fn.abs_lineinfo = {}
        for i = 1, abs_lineinfo_count do
            local linfo = {}
            linfo.pc = self:next_size()
            linfo.line = self.next_size()
            fn.abs_lineinfo[i] = linfo
        end

        local locvars_count = self:next_size()
        fn.locvars = {}
        for i = 1, locvars_count do
            local locvar = {}
            locvar.varname = self:next_string()
            locvar.startpc = self:next_size()
            locvar.endpc = self:next_size()
            fn.locvars[i] = locvar
        end

        local upvals_count = self:next_size()
        fn.upvals = {}
        for i = 1, upvals_count do
            local upval = {}
            upval.name = self:next_string()
            fn.upvals[i] = upval
        end

        return fn
    end
    return parser
end

function Minmus.new_interpreter(parsed_fn)
    local interpreter = {}
    interpreter.frame = {
        fn = parsed_fn,
        pc = 1,
        regs = {},
        top = parsed_fn.max_stack_size,
        prev_frame = nil,
    }
    interpreter.last_ret = nil

    function interpreter:e_ABC(ins)
        local a = (ins >> 7) & 0xff
        local b = (ins >> (8+8)) & 0xff
        local c = (ins >> (8+8+8)) & 0xff
        return a, b, c
    end

    function interpreter:e_AsBx(ins)
        local a = (ins >> 7) & 0xff
        local offset = ((1<<17)-1) >> 1
        local b = (ins >> (7+8)) - offset
        return a, b
    end
    
    function interpreter:e_k(ins)
        return (ins >> (7+8)) & 1
    end

    interpreter[1]  = function(self, ins) -- OP_LOADI
        local a, b = self:e_AsBx(ins)
        self.frame.regs[a] = {
            val = b
        }
    end

    interpreter[34] = function(self, ins) -- OP_ADD
        local a, b, c = self:e_ABC(ins)
        local regs = self.frame.regs
        regs[a] = {
            val = regs[b].val + regs[c].val
        }
        self.frame.pc = self.frame.pc + 1 -- TODO: metatable support
    end
    
    interpreter[70] = function(self, ins) -- OP_RETURN
        local a, b, c = self:e_ABC(ins)
        local k = self:e_k(ins)
        print(a, b, c)
        if b == 0 then
            assert(false) -- TODO
        else
            local retvals = {}
            for i = 1, b-1 do
                retvals[i] = self.frame.regs[i-1+a]
            end
            self:ret(retvals)
        end
    end

    interpreter[81] = function(self, ins) -- OP_VARARGPREP
        -- nothing for now
    end

    function interpreter:ret(retvals)
        --print("Retval", Minmus.dump(retvals))
        self.frame = self.frame.prev_frame
        self.last_ret = retvals
    end

    function interpreter:step()
        local frame = self.frame
        local ins = frame.fn.code[frame.pc]
        local opcode = ins & 127
        print("Opcode", opcode, ins)
        frame.pc = frame.pc + 1
        interpreter[opcode](interpreter, ins)
        --print(Minmus.dump(frame.regs))
    end

    return interpreter
end

function Minmus.parse(dump)
    local parser = Minmus.new_parser(dump)
    local info = parser.info

    -- Parse header
    parser:skip_bytes(4) -- SIGNATURE
    parser:skip_bytes(1) -- VERSION
    assert(parser:next_byte() == 0) -- FORMAT
    parser:skip_bytes(6) -- DATA
    assert(parser:next_byte() == 4) -- instruction size
    info.ins_size = 4
    info.int_size = parser:next_byte() -- int size
    info.num_size = parser:next_byte() -- number size
    assert(parser:next_int() == 0x5678)
    assert(parser:next_num() == 370.5)

    -- 
    parser:next_byte() -- sizeupvalues (?)
    local res = parser:next_fn()
    --print(Minmus.dump(res))
    --print(dump:sub(parser.pointer))
    return res
end

function Minmus.compile(code)
    local parsed = Minmus.parse(string.dump(load(code)))
    local interpreter = Minmus.new_interpreter(parsed)
    while interpreter.frame ~= nil do
        interpreter:step()
    end
    print(Minmus.dump(interpreter.last_ret))
end

--Minmus.parse(string.dump(load("return 'asdfasdf'..tostring(1+2+3)")))
if false then
    Minmus.parse(string.dump(load([[
        local a = 5
        b = 3
        function lalala()
            return a + b
        end
        return lalala()
    ]])))
end
Minmus.compile([[
    local a = 1
    local b = 2
    local c = a + b
    return c
    ]])
