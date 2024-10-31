# ~/.irbrc

IRB.conf[:PROMPT][:MY_PROMPT] = { # name of prompt mode
  :AUTO_INDENT => true,           # enables auto-indent mode
  :PROMPT_I =>  ">> ",            # simple prompt
  :PROMPT_S => nil,               # prompt for continuated strings
  :PROMPT_C => nil,               # prompt for continuated statement
  :RETURN => "==>%s\n"         # format to return value
}

IRB.conf[:PROMPT_MODE] = :MY_PROMPT

def ri(*names)
  system(%{ri #{names.map {|name| name.to_s}.join(" ")}})
end

def reset
  puts "Restarting IRB"
  exec $0
end
