#!/usr/local/bin/julia

comp_version = "0.9.0"

# read operations list as argument
args = ARGS

#=

    note: base data structure is a vector (linked
    list) used as a stack. atoms on the list are
    either be symbols (commands) or values. each
    calculation is a list of operations that are
    processed in order of occurrence. this is an
    implementation of a lisp intepreter for rev-
    erse polish notation s-expressions (sexp).

      operations list structure
        (object : command or value)
        "5"
        ":sqrt"
        "1
        ":-"
        "2"
        ":/"

    a list evaluation engine takes the list of
    strings and executes the corresponding oper-
    ations then returns the resulting mutated
    stack.

=#

function main(oplist::Vector{String})
  # create computation stack
  cstack = Vector{Float64}(undef, 0)

  # evaluate list of arguments and update stack by
  # mapping node evaluation to the operations list
  map(x -> process_node!(cstack, x), oplist)

  # return result of argument list evaluation
  println(string(cstack, "\n"))
end

struct Command
  symbol::String
  command_f::Symbol # command function
end

# execute command function for command or value
function process_node!(stack::Vector{Float64}, cmd_or_val::String)
  f = getcmdfunction(cmd_or_val)
  eval(f)(stack, cmd_or_val)
  return nothing
end

# get command function
function getcmdfunction(sinput::String)
  for c in commands
    if c.symbol == sinput
      return c.command_f
    end
  end
  # if not symbol add value to stack
  return :c_addtostack!
end

function c_addtostack!(s::Vector{Float64}, cov::String)
  push!(s, parse(Float64, cov))
  return nothing
end

# build command vector
commands = Vector{Command}(undef, 0)

# - add
push!(commands, Command(":+", :c_add!))
function c_add!(s::Vector{Float64}, cov::String)
  s[end-1] += pop!(s)
  return nothing
end

# - subtract
push!(commands, Command(":-", :c_sub!))
function c_sub!(s::Vector{Float64}, cov::String)
  s[end-1] -= pop!(s)
  return nothing
end

# - multiply
push!(commands, Command(":x", :c_mult!))
function c_mult!(s::Vector{Float64}, cov::String)
  s[end-1] *= pop!(s)
  return nothing
end

# - divide
push!(commands, Command(":/", :c_div!))
function c_div!(s::Vector{Float64}, cov::String)
  s[end-1] /= pop!(s)
  return nothing
end

# - inverse division
push!(commands, Command(":\\", :c_inversedivide!))
function c_inversedivide!(s::Vector{Float64}, cov::String)
  s[end-1] \= pop!(s)
  return nothing
end

# - square root
push!(commands, Command(":sqrt", :c_sqrt!))
function c_sqrt!(s::Vector{Float64}, cov::String)
  s[end] = sqrt( s[end] )
  return nothing
end

# - invert
push!(commands, Command(":inv", :c_inv!))
function c_inv!(s::Vector{Float64}, cov::String)
  s[end] = 1 / s[end]
  return nothing
end

# - change sign
push!(commands, Command(":chs", :c_chs!))
function c_chs!(s::Vector{Float64}, cov::String)
  s[end] = -1 * s[end]
  return nothing
end

# - exponent - power
push!(commands, Command(":exp", :c_exp!))
function c_exp!(s::Vector{Float64}, cov::String)
  s[end-1] ^= pop!(s)
  return nothing
end

# - duplicate
push!(commands, Command(":dup", :c_dup!))
function c_dup!(s::Vector{Float64}, cov::String)
  push!(s, s[end])
  return nothing
end

# - reverse x and y
push!(commands, Command(":rev", :c_reverse!))
function c_reverse!(s::Vector{Float64}, cov::String)
  x = s[end]
  s[end] = s[end-1]
  s[end-1] = x
  return nothing
end

# - modulus
push!(commands, Command(":%", :c_mod!))
function c_mod!(s::Vector{Float64}, cov::String)
  divisor = pop!(s)
  s[end] = s[end] % divisor
  return nothing
end

# - sine
push!(commands, Command(":sin", :c_sin!))
function c_sin!(s::Vector{Float64}, cov::String)
  s[end] = sin( s[end] )
  return nothing
end

# - cosine
push!(commands, Command(":cos", :c_cos!))
function c_cos!(s::Vector{Float64}, cov::String)
  s[end] = cos( s[end] )
  return nothing
end

# - tangent
push!(commands, Command(":tan", :c_tan!))
function c_tan!(s::Vector{Float64}, cov::String)
  s[end] = tan( s[end] )
  return nothing
end

# - arcsine
push!(commands, Command(":asin", :c_asin!))
function c_asin!(s::Vector{Float64}, cov::String)
  s[end] = asin( s[end] )
  return nothing
end

# - arccosine
push!(commands, Command(":acos", :c_acos!))
function c_acos!(s::Vector{Float64}, cov::String)
  s[end] = acos( s[end] )
  return nothing
end

# - arctangent
push!(commands, Command(":atan", :c_atan!))
function c_atan!(s::Vector{Float64}, cov::String)
  s[end] = atan( s[end] )
  return nothing
end

# - pi
push!(commands, Command(":pi", :c_pi!))
function c_pi!(s::Vector{Float64}, cov::String)
  push!(s, pi)
  return nothing
end

# - Euler's number
push!(commands, Command(":e", :c_euler!))
function c_euler!(s::Vector{Float64}, cov::String)
  push!(s, ℯ)
  return nothing
end

# - log 10
push!(commands, Command(":log", :c_log10!))
function c_log10!(s::Vector{Float64}, cov::String)
  s[end] = log10( s[end] )
  return nothing
end

# - natural log
push!(commands, Command(":ln", :c_ln!))
function c_ln!(s::Vector{Float64}, cov::String)
  s[end] = log( s[end] )
  return nothing
end

# - degrees to radians
push!(commands, Command(":dtor", :c_dtor!))
function c_dtor!(s::Vector{Float64}, cov::String)
  s[end] = s[end] * pi / 180
  return nothing
end

# - radians to degrees
push!(commands, Command(":rtod", :c_rtod!))
function c_rtod!(s::Vector{Float64}, cov::String)
  s[end] = s[end] * 180 / pi
  return nothing
end

# - factorial
push!(commands, Command(":!", :c_factorial!))
function c_factorial!(s::Vector{Float64}, cov::String)
  s[end] = factorial( Int64(s[end]) )
  return nothing
end

# - absolute value
push!(commands, Command(":abs", :c_abs!))
function c_abs!(s::Vector{Float64}, cov::String)
  s[end] = abs( s[end] )
  return nothing
end


if length(args) == 0
  push!(args, "help")
end

if  args[1] == "--help" || args[1] == "help"
  println()
  println("comp ", comp_version)
  println()
  println("usage: comp <list>")
  println()
  println("  <list> contains a sequence of operations (commands or values)")
  println()
  println("  for example, 'comp 5 :sqrt 1 :- 2 :/' calculates the golden ratio")
  println()
  println("commands")
  for c in commands
    print(string(c.symbol), "  ")
  end
  println("\n")
  return
elseif args[1] == "--version" || args[1] == "version"
  println("comp ", comp_version)
elseif args[1] == "mona"
  println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!>''''''<!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!'''''`             ``'!!!!!!!!!!!!!!!!!!!!!!!!")
  println("!!!!!!!!!!!!!!!!!!!!!!!!''`          .....         `'!!!!!!!!!!!!!!!!!!!!!")
  println("!!!!!!!!!!!!!!!!!!!!!'`      .      :::::'            `'!!!!!!!!!!!!!!!!!!")
  println("!!!!!!!!!!!!!!!!!!!'     .   '     .::::'                `!!!!!!!!!!!!!!!!")
  println("!!!!!!!!!!!!!!!!!'      :          `````                   `!!!!!!!!!!!!!!")
  println("!!!!!!!!!!!!!!!!        .,cchcccccc,,.                       `!!!!!!!!!!!!")
  println("!!!!!!!!!!!!!!!     .-\"?\$\$\$\$\$\$\$\$\$\$\$\$\$\$c,                      `!!!!!!!!!!!")
  println("!!!!!!!!!!!!!!    ,ccc\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$,                     `!!!!!!!!!!")
  println("!!!!!!!!!!!!!    z\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$;.                    `!!!!!!!!!")
  println("!!!!!!!!!!!!    <\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$:.                    `!!!!!!!!")
  println("!!!!!!!!!!!     \$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$h;:.                   !!!!!!!!")
  println("!!!!!!!!!!'     \$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$h;.                   !!!!!!!")
  println("!!!!!!!!!'     <\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$                   !!!!!!!")
  println("!!!!!!!!'      `\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$F                   `!!!!!!")
  println("!!!!!!!!        c\$\$\$\$???\$\$\$\$\$\$\$P\"\"  \"\"\"??????\"                      !!!!!!")
  println("!!!!!!!         `\"\" .,.. \"\$\$\$\$F    .,zcr                            !!!!!!")
  println("!!!!!!!         .  dL    .?\$\$\$   .,cc,      .,z\$h.                  !!!!!!")
  println("!!!!!!!!        <. \$\$c= <\$d\$\$\$   <\$\$\$\$=-=+\"\$\$\$\$\$\$\$                  !!!!!!")
  println("!!!!!!!         d\$\$\$hcccd\$\$\$\$\$   d\$\$\$hcccd\$\$\$\$\$\$\$F                  `!!!!!")
  println("!!!!!!         ,\$\$\$\$\$\$\$\$\$\$\$\$\$\$h d\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$                   `!!!!!")
  println("!!!!!          `\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$<\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$'                    !!!!!")
  println("!!!!!          `\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\"\$\$\$\$\$\$\$\$\$\$\$\$\$P>                     !!!!!")
  println("!!!!!           ?\$\$\$\$\$\$\$\$\$\$\$\$??\$c`\$\$\$\$\$\$\$\$\$\$\$?>'                     `!!!!")
  println("!!!!!           `?\$\$\$\$\$\$I7?\"\"    ,\$\$\$\$\$\$\$\$\$?>>'                       !!!!")
  println("!!!!!.           <<?\$\$\$\$\$\$c.    ,d\$\$?\$\$\$\$\$F>>''                       `!!!")
  println("!!!!!!            <i?\$P\"??\$\$r--\"?\"\"  ,\$\$\$\$h;>''                       `!!!")
  println("!!!!!!             \$\$\$hccccccccc= cc\$\$\$\$\$\$\$>>'                         !!!")
  println("!!!!!              `?\$\$\$\$\$\$F\"\"\"\"  `\"\$\$\$\$\$>>>''                         `!!")
  println("!!!!!                \"?\$\$\$\$\$cccccc\$\$\$\$??>>>>'                           !!")
  println("!!!!>                  \"\$\$\$\$\$\$\$\$\$\$\$\$\$F>>>>''                            `!")
  println("!!!!!                    \"\$\$\$\$\$\$\$\$???>'''                                !")
  println("!!!!!>                     `\"\"\"\"\"                                        `")
  println("!!!!!!;                       .                                          `")
  println("!!!!!!!                       ?h.")
  println("!!!!!!!!                       \$\$c,")
  println("!!!!!!!!>                      ?\$\$\$h.              .,c")
  println("!!!!!!!!!                       \$\$\$\$\$\$\$\$\$hc,.,,cc\$\$\$\$\$")
  println("!!!!!!!!!                  .,zcc\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$")
  println("!!!!!!!!!               .z\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$")
  println("!!!!!!!!!             ,d\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$          .")
  println("!!!!!!!!!           ,d\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$         !!")
  println("!!!!!!!!!         ,d\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$        ,!'")
  println("!!!!!!!!>        c\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$.       !'")
  println("!!!!!!''       ,d\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$ ( by Allen Mullen )")
else
  main(args)
end
