
table.insert(data.techs.t_signals2.unlocks, "c_quant_lua_behavior_controller")

QuantG = {}

-- define and register a custom component, the ID needs to be unique
local c_quant_lua_behavior_controller = Comp:RegisterComponent("c_quant_lua_behavior_controller", {
	name = "Lua Behavior Controller",
	desc = "Teleport unit to location specified in register",
	texture = "Main/textures/icons/components/Component_UnitTeleporter_01_L.png",
	slot_type = "storage",
	attachment_size = "Internal",
	visual = "v_generic_i",
	activation = "Always",
	registers = { { tip = "Coordinate" } },
    production_recipe = CreateProductionRecipe({ circuit_board = 1 }, { c_assembler = 5 }),
	action_tooltip = "Open Editor",
})

function c_quant_lua_behavior_controller:action_click(comp)
    QuantG:MultilineInputBox("Enter the text for the sign post", "Sign Post",
    function (t) Action.SendForEntity("QuantLuaEdit", comp.owner, { comp = comp, new_code = t }) end,
    comp.extra_data.code or "")
end

function EntityAction.QuantLuaEdit(entity, arg)
	local ed = arg.comp.extra_data
    ed.code = arg.new_code
	local env = {
		print = {
			kind = Minmus.kinds.NATIVEFN,
			val = "print",
		}
	}
    ed.interpreter_info = Minmus.compile(ed.code, nil):save()
	print(Minmus.dump(ed.interpreter_info))
	ed.interpreter_info = nil
end

-- callback called when the component is activated
function c_quant_lua_behavior_controller:on_update(comp)
	local ed = comp.extra_data
	local step_ctx = {
		native_fns = {
			print = print,
		},
	}
	--if ed.interpreter ~= nil then
	--	ed.interpreter:step()
	--end
	--if interpreter.frame == nil then
	--	ed.interpreter = nil
	--	print("Program finished")
	--end
    --ed.func()
    --ed.func = nil
    --local coord = comp:GetRegisterCoord(1)
	--if coord then
		-- have a coordinate, teleport the owner there
		-- Map.Defer(function() comp.owner:Place(coord.x, coord.y) end)
	--end
end

function QuantG:MultilineInputBox(msg, title, on_ok, current_text, password, disallow_empty)
	UI.AddLayout("ConfirmDialog", {
		title = title, body = msg,
		construct = function(w)
			local i = w.list:Add("MultilineInputText", {height=500})
            i.fill = true
			w.input, i.text, i.password, i.on_enter = i, current_text or "", (password ~= nil), function() w:ok() end
			i:Focus()
		end,
		ok = function(w) if on_ok(w.input.text) ~= false then w:RemoveFromParent() end end,
		cancel = function(w) w:RemoveFromParent() end,
	}, 99)
end