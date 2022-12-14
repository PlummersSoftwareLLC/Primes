

local statement = function(self, lex)
    
    lex:expect"macro"
    local vname = lex:expect(lex.name).value
    local ret = {}
    local tname;
    while lex:nextif"." do
        tname = vname
        vname = lex:expect(lex.name).value
        if not ret[1] then
            ret[1] = {}
            table.insert(ret[1], tname)
        end
        table.insert(ret[1] or ret, vname)
    end
    if lex:nextif":" then
        tname = vname
        vname = lex:expect(lex.name).value
        if not ret[1] then
            ret[1] = {}
            table.insert(ret[1], tname)
        end
        table.insert(ret[1] or ret, "methods")
        table.insert(ret[1] or ret, vname)
    end
    
    ret[1] = ret[1] or vname
    lex:expect"("
    local expl = terralib.newlist();
    repeat
        if not lex:matches(lex.name) then
            break;
        end
        local expr = lex:expect(lex.name).value
        expl:insert(expr)
    until not lex:nextif","
    
    lex:expect")"
    local expfn = lex:luastats()
    lex:expect"end"
    return function(environment_function)
        local env = environment_function()
        local m = macro( function(...)
            local arg = {...}
            local j = 1
            if tname then
                j = 2
                env.self = arg[1]
                
            end
            
            for i = 1, #expl do
                env[expl[i]] = arg[j]
                j = j + 1
            end
            return expfn(env)
        end)
        return m
    end, ret
end;

return {
    name = "macro";
    entrypoints = {"macro"};
    keywords = {};
    expression = function(self,lex)
		lex:expect("macro")
		lex:expect("(")
        
        local expl = terralib.newlist();
        repeat
		    local expr = lex:expect(lex.name).value
            expl:insert(expr)
        until not lex:nextif","
        
		lex:expect")"
		local expfn = lex:luastats()
        lex:expect"end"
		return function(environment_function)
			
			return macro( function(...)
				local env = environment_function()
                local arg = {...}
                
				for i = 1, #expl do
				    env[expl[i]] = arg[i]
                    
                end
				return expfn(env)
			end)
		end
	end;

    statement = statement;
    localstatement = statement;
}