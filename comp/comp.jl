#!julia

const COMP_VERSION = "0.10.1"

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
  println(string(cstack))
end

# execute command function for command or value
function process_node!(stack::Vector{Float64}, cmd_or_val::String)
  f = getcmdfunction(cmd_or_val)
  eval(f)(stack, cmd_or_val)
  return nothing
end

# get command function
function getcmdfunction(sinput::String)
  # if not symbol add value to stack
  sinput[1] == ':' ? (return commands[sinput]) : (return :c_addtostack!)
end

function c_addtostack!(s::Vector{Float64}, cov::String)
  push!(s, parse(Float64, cov))
  return nothing
end


# create command dictionary and build out commands
commands = Dict{String, Symbol}()

# - add
commands[":+"] = :c_add!!
function c_add!(s::Vector{Float64}, cov::String)
  s[end-1] += pop!(s)
  return nothing
end

# - subtract
commands[":-"] = :c_sub!
function c_sub!(s::Vector{Float64}, cov::String)
  s[end-1] -= pop!(s)
  return nothing
end

# - multiply
commands[":x"] = :c_mult!
function c_mult!(s::Vector{Float64}, cov::String)
  s[end-1] *= pop!(s)
  return nothing
end

# - divide
commands[":/"] = :c_div!
function c_div!(s::Vector{Float64}, cov::String)
  s[end-1] /= pop!(s)
  return nothing
end

# - inverse division
commands[":\\"] = :c_inversedivide!
function c_inversedivide!(s::Vector{Float64}, cov::String)
  s[end-1] \= pop!(s)
  return nothing
end

# - square root
commands[":sqrt"] = :c_sqrt!
function c_sqrt!(s::Vector{Float64}, cov::String)
  s[end] = sqrt( s[end] )
  return nothing
end

# - invert
commands[":inv"] = :c_inv!
function c_inv!(s::Vector{Float64}, cov::String)
  s[end] = 1 / s[end]
  return nothing
end

# - change sign
commands[":chs"] = :c_chs!
function c_chs!(s::Vector{Float64}, cov::String)
  s[end] = -1 * s[end]
  return nothing
end

# - exponent - power
commands[":exp"] = :c_exp!
function c_exp!(s::Vector{Float64}, cov::String)
  s[end-1] ^= pop!(s)
  return nothing
end

# - duplicate
commands[":dup"] = :c_dup!
function c_dup!(s::Vector{Float64}, cov::String)
  push!(s, s[end])
  return nothing
end

# - reverse x and y
commands[":rev"] = :c_reverse!
function c_reverse!(s::Vector{Float64}, cov::String)
  x = s[end]
  s[end] = s[end-1]
  s[end-1] = x
  return nothing
end

# - modulus
commands[":%"] = :c_mod!
function c_mod!(s::Vector{Float64}, cov::String)
  divisor = pop!(s)
  s[end] = s[end] % divisor
  return nothing
end

# - sine
commands[":sin"] = :c_sin!
function c_sin!(s::Vector{Float64}, cov::String)
  s[end] = sin( s[end] )
  return nothing
end

# - cosine
commands[":cos"] = :c_cos!
function c_cos!(s::Vector{Float64}, cov::String)
  s[end] = cos( s[end] )
  return nothing
end

# - tangent
commands[":tan"] = :c_tan!
function c_tan!(s::Vector{Float64}, cov::String)
  s[end] = tan( s[end] )
  return nothing
end

# - arcsine
commands[":asin"] = :c_asin!
function c_asin!(s::Vector{Float64}, cov::String)
  s[end] = asin( s[end] )
  return nothing
end

# - arccosine
commands[":acos"] = :c_acos!
function c_acos!(s::Vector{Float64}, cov::String)
  s[end] = acos( s[end] )
  return nothing
end

# - arctangent
commands[":atan"] = :c_atan!
function c_atan!(s::Vector{Float64}, cov::String)
  s[end] = atan( s[end] )
  return nothing
end

# - pi
commands[":pi"] = :c_pi!
function c_pi!(s::Vector{Float64}, cov::String)
  push!(s, pi)
  return nothing
end

# - Euler's number
commands[":e"] = :c_euler!
function c_euler!(s::Vector{Float64}, cov::String)
  push!(s, ℯ)
  return nothing
end

# - log 10
commands[":log"] = :c_log10!
function c_log10!(s::Vector{Float64}, cov::String)
  s[end] = log10( s[end] )
  return nothing
end

# - natural log
commands[":ln"] = :c_ln!
function c_ln!(s::Vector{Float64}, cov::String)
  s[end] = log( s[end] )
  return nothing
end

# - degrees to radians
commands[":dtor"] = :c_dtor!
function c_dtor!(s::Vector{Float64}, cov::String)
  s[end] = s[end] * pi / 180
  return nothing
end

# - radians to degrees
commands[":rtod"] = :c_rtod!
function c_rtod!(s::Vector{Float64}, cov::String)
  s[end] = s[end] * 180 / pi
  return nothing
end

# - factorial
commands[":!"] = :c_factorial!
function c_factorial!(s::Vector{Float64}, cov::String)
  s[end] = factorial( Int64(s[end]) )
  return nothing
end

# - absolute value
commands[":abs"] = :c_abs!
function c_abs!(s::Vector{Float64}, cov::String)
  s[end] = abs( s[end] )
  return nothing
end


# handle arguments

args = ARGS
length(args) == 0 ? push!(args, "help") : nothing

if  args[1] == "--help" || args[1] == "help"
  println("usage: comp <list>")
  println("            [version] [help]")
  println()
  println("The <list> is a sequence of reverse Polish notion (RPN) operations. Each operation is either a command (symbol) or value. For example, 'comp 1 2 :+' adds the numbers 1 and 2, and 'comp 5 :sqrt 1 :- 2 :/' calculates the golden ratio. The available commands are listed below.")
  println()
  println("commands")
  for c in keys(commands)
    print(c, " ")
  end
elseif args[1] == "--version" || args[1] == "version"
  println("comp ", COMP_VERSION)
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
  println("!!!!!!''       ,d\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$\$       allen mullen ")
else
  main(args)
end
