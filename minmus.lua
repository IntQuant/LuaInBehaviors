-- Notes:
-- fn.protos and fn.constants and fn.upvals are zero-indexed
-- frame.upvals and frame.regs are zero-indexed
-- frame.top is the first *free* index

Minmus = {}

local function makevariant(base, sub)
    return  base | (sub << 4)
end

Minmus.kinds = {
    CLOSURE = "closure"
}

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
function Minmus.dump(o, level)
    if level == nil then
        level = 5
    end
    if type(o) == 'table' then
        if level < 0 then
            return "<table>"
        end
        local s = '{ '
        for k,v in pairs(o) do
            if k == "code_str" then 
                s = s .. "<" .. k .. ">, " 
            else
                if type(k) ~= 'number' then k = '"'..k..'"' end
                s = s .. '['..k..'] = ' .. Minmus.dump(v, level-1) .. ', '
            end
        end
        return s .. '} '
    else
        return tostring(o)
    end
end

function Minmus.dump_regs(regs, name)
    local name = name or "regs"
    print("Current", name)
    for i = 0, #regs do
        if regs[i] ~= nil then
            if regs[i].kind == Minmus.kinds.CLOSURE then 
                print(i, "kind", regs[i].kind)
            else
                print(i, Minmus.dump(regs[i]))
            end
        end
    end
    print("End of", name)
end

function Minmus.copyval(from, to)
    to.val = from.val
    to.kind = from.kind
    to.proto = from.proto    
end

function Minmus.is_int(val)
    return math.type(val) == 'integer'
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

            constants[i-1] = const
        end
        fn.constants = constants

        -- upvals
        local upval_count = self:next_size()
        local upvals = {}
        for i = 1, upval_count do
            local upval = {}
            upval.instack = self:next_byte() > 0
            upval.idx = self:next_byte()
            upval.kind = self:next_byte()
            upvals[i-1] = upval
        end
        fn.upvals = upvals

        -- protos
        local protos_count = self:next_size()
        local protos = {}
        for i = 1, protos_count do
            protos[i-1] = parser:next_fn()
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
        for i = 1, upvals_count do
            fn.upvals[i-1].name = self:next_string()
        end
        
        print(Minmus.dump(fn.upvals))
        return fn
    end
    return parser
end

function Minmus.new_frame(parsed_fn, prev_frame)
    local upvals = {}
    Minmus.dump_regs(prev_frame.regs)
    if parsed_fn.upvals[0] ~= nil then
        for i = 0, #parsed_fn.upvals do
            local upval_info = parsed_fn.upvals[i]
            print("Creating upval", i, Minmus.dump(upval_info))
            if upval_info.instack then
                upvals[i] = prev_frame.regs[upval_info.idx]
            else
                upvals[i] = prev_frame.upvals[upval_info.idx]
            end
        end
    end
    --Minmus.dump_regs(upvals, "upvals")
    --Minmus.dump_regs(prev_frame.upvals, "prev upvals")
    
    return {
        fn = parsed_fn,
        pc = 1,
        regs = {},
        top = parsed_fn.max_stack_size,
        prev_frame = prev_frame,
        upvals = upvals,
    }
end

function Minmus.new_interpreter(parsed_fn)
    local interpreter = {}
    local pseudoframe = {
        regs = {[0] = {}},
        upvals = {},
        is_pseudoframe = true,
    }
    interpreter.frame = Minmus.new_frame(parsed_fn, pseudoframe)
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

    function interpreter:e_ABx(ins)
        local a = (ins >> 7) & 0xff
        local b = (ins >> (7+8))
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

    interpreter[9] = function(self, ins) -- OP_GETUPVAL
        local a, b, c = self:e_ABC(ins)
        print("Get upval", b, a)
        self.frame.regs[a] = self.frame.upvals[b]
        --print(Minmus.dump(self.frame.upvals))
    end

    interpreter[10] = function(self, ins) -- OP_SETUPVAL
        local a, b, c = self:e_ABC(ins)
        local val = self.frame.regs[a]
        local upval = self.frame.upvals[b]
        Minmus.copyval(val, upval)
    end

    interpreter[11] = function(self, ins) -- OP_GETTABUP
        local a, b, c = self:e_ABC(ins)
        local key = self.frame.fn.constants[c].val
        print("Get upval", b, key, a)
        self.frame.regs[a] = self.frame.upvals[b][key]
        --print(Minmus.dump(self.frame.upvals))
    end

    interpreter[15] = function(self, ins) -- OP_SETTABUP
        local a, b, c = self:e_ABC(ins)
        local k = self:e_k(ins)
        local val
        if k > 0 then
            val = self.frame.fn.constants[c]
        else
            val = self.frame.regs[c]
        end
        local key = self.frame.fn.constants[b].val
        print("Set upval", a, key)
        self.frame.upvals[a][key] = val -- TODO figure out keys
        --print(Minmus.dump(self.frame.upvals))
    end

    interpreter[34] = function(self, ins) -- OP_ADD
        Minmus.dump_regs(self.frame.regs)
        local a, b, c = self:e_ABC(ins)
        local regs = self.frame.regs
        regs[a] = {
            val = regs[b].val + regs[c].val
        }
        self.frame.pc = self.frame.pc + 1 -- TODO: metatable support
    end
    
    interpreter[68] = function(self, ins) -- OP_CALL
        print("Call")
        local a, b, c = self:e_ABC(ins)
        if b == 0 then
            b = self.frame.top - a
        end
        self.frame.ret_info = {a, c}
        local cls = self.frame.regs[a]
        assert(cls.kind == Minmus.kinds.CLOSURE)
        local new_frame = Minmus.new_frame(cls.proto, self.frame)
        new_frame.prev_frame = self.frame
        self.frame = new_frame
        --print(Minmus.dump(self.frame))
    end

    interpreter[69] = function(self, ins) -- OP_TAILCALL -- TODO figure out k (closing upvals)
        self[68](self, ins)
        self.frame.prev_frame = self.frame.prev_frame.prev_frame
    end

    interpreter[70] = function(self, ins) -- OP_RETURN -- TODO figure out k (closing upvals)
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
    interpreter[71] = function(self, ins) -- OP_RETURN0
        self:ret({})
    end
    interpreter[72] = function(self, ins) -- OP_RETURN1
        local a, b, c = self:e_ABC(ins)
        self:ret({self.frame.regs[a]})
    end
    
    interpreter[73] = function(self, ins) -- OP_FORLOOP
        local a, b = self:e_ABx(ins)
        local regs = self.frame.regs
        
        local init = regs[a].val
        local limit = regs[a+1].val
        local step = regs[a+2].val
        
        local stepsign = 0 < step
        if (stepsign and limit <= init) or (not stepsign and limit >= init) then
            return
        else
            init = init + step
            regs[a].val = init
            regs[a+3].val = init
            self.frame.pc = self.frame.pc - b
        end
    end

    interpreter[74] = function(self, ins) -- OP_FORPREP
        local a, b = self:e_ABx(ins)
        local regs = self.frame.regs
        
        local init = tonumber(regs[a].val)
        local limit = tonumber(regs[a+1].val)
        local step = tonumber(regs[a+2].val)
        if init == nil or limit == nil or step == nil then
            self:err("could not convert 'for' vars to numbers")
        end
        if step == 0 then
            self:err("'for' step is zero")
        end
        
        local stepsign = 0 < step
        if (stepsign and limit < init) or (not stepsign and limit > init) then
            self.frame.pc = self.frame.pc + b + 1
            return
        end

        regs[a] = {
            val = init
        }
        regs[a+1] = {
            val = limit
        }
        regs[a+2] = {
            val = step
        }
        regs[a+3] = {
            val = init
        }
    end


    interpreter[79] = function(self, ins) -- OP_CLOSURE
        local a, b = self:e_ABx(ins)
        self.frame.regs[a] = {
            kind = Minmus.kinds.CLOSURE,
            proto = self.frame.fn.protos[b],
            --upvals = self.frame.upvals,
        }
    end

    interpreter[81] = function(self, ins) -- OP_VARARGPREP
        -- nothing for now
    end

    function interpreter:ret(retvals)
        --print("Retval", Minmus.dump(retvals))
        self.last_ret = retvals
        if self.frame.prev_frame.is_pseudoframe then
            self.frame = nil
            return -- no more code left to run
        end
        self.frame = self.frame.prev_frame
        
        -- copy retvals to registers
        local a = self.frame.ret_info[1]
        local c = self.frame.ret_info[2]

        assert(c ~= 0) -- TODO
        local regs = self.frame.regs
        for i = 0, c-2 do
            regs[a+i] = retvals[i+1]
        end
    end

    function interpreter:err(err)
        print(err)
        assert(false)
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
    return Minmus.new_interpreter(parsed)
end

function Minmus.compile_and_run(code)
    local interpreter = Minmus.compile(code)
    while interpreter.frame ~= nil do
        interpreter:step()
    end
    print(Minmus.dump(interpreter.last_ret))
    return interpreter.last_ret
end

--Minmus.parse(string.dump(load("return 'asdfasdf'..tostring(1+2+3)")))
