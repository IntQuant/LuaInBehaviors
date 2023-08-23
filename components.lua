
table.insert(data.techs.t_signals2.unlocks, "c_quant_lua_behavior_controller")

-- define and register a custom component, the ID needs to be unique
local c_quant_lua_behavior_controller = Comp:RegisterComponent("c_quant_lua_behavior_controller", {
	name = "Lua Behavior Controller",
	desc = "Teleport unit to location specified in register",
	texture = "Main/textures/icons/components/Component_UnitTeleporter_01_L.png",
	slot_type = "storage",
	attachment_size = "Internal",
	visual = "v_generic_i",
	activation = "OnFirstRegisterChange",
	registers = { { tip = "Coordinate" } },
    production_recipe = CreateProductionRecipe({ circuit_board = 1 }, { c_assembler = 5 }),
	action_tooltip = "Open Editor",
})

function c_quant_lua_behavior_controller:action_click(comp)
    MultilineInputBox("Enter the text for the sign post", "Sign Post",
    function (t) Action.SendForEntity("QuantLuaEdit", comp.owner, { comp = comp, sign = t }) end,
    comp.owner.extra_data.signpost or "")
end

function EntityAction.QuantLuaEdit(entity, arg)
	if not arg.sign or arg.sign == "" then
		if arg.comp then arg.comp.extra_data.signpost = nil end
		entity.extra_data.signpost = nil
		if entity:GetRegisterId(FRAMEREG_VISUAL) == "c_signpost" then entity:SetRegister(FRAMEREG_VISUAL, nil) end
	else
		if arg.comp then arg.comp.extra_data.signpost = arg.sign end
		entity.extra_data.signpost = arg.sign
		entity:SetRegisterId(FRAMEREG_VISUAL, "c_signpost")
	end
end

-- callback called when the component is activated
function c_quant_lua_behavior_controller:on_update(comp)
	local coord = comp:GetRegisterCoord(1)
	if coord then
		-- have a coordinate, teleport the owner there
		Map.Defer(function() comp.owner:Place(coord.x, coord.y) end)
	end
end

function MultilineInputBox(msg, title, on_ok, current_text, password, disallow_empty)
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