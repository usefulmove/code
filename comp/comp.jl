#!/usr/bin/julia

comp_version = "0.8.1"

# read operations list as argument
args = ARGS

#=

    note: base data structure is a vector (linked
    list) used as a stack. atoms on the list are
    either be symbols (commands) or values. each
    calculation is a list of operations that are
    processed in order of occurrence. this is an
    implementation of a Lisp construct.

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

function main(oplist)
  # create computation stack
  cstack = Vector{Float64}(undef, 0)

  # evaluate list of arguments and update stack by
  # mapping node evaluation to the operations list
  map(x -> process_node!(cstack, x), oplist)

  # return result of argument list evaluation
  println(string(cstack, "\r"))
end

struct Command
  symbol::String
  command_f # command function
end

# operation execution
function process_node!(stack, cmd_or_val)
  # execute command function for command or value
  f = cmdfunction(cmd_or_val)
  eval(f)(stack, cmd_or_val)
end

# get command function
function cmdfunction(sinput)
  for c in commands
    if c.symbol == sinput
      return c.command_f
    end
  end
  # if not symbol add value to stack
  return :c_addtostack!
end

function c_addtostack!(s, cov)
  push!(s, parse(Float64, cov))
end

# build command vector
commands = Vector{Command}(undef, 0)

# - add
push!(commands, Command(":+", :c_add!))
function c_add!(s, cov)
  s[end-1] += pop!(s)
end

# - subtract
push!(commands, Command(":-", :c_sub!))
function c_sub!(s, cov)
  s[end-1] -= pop!(s)
end

# - multiply
push!(commands, Command(":x", :c_mult!))
function c_mult!(s, cov)
  s[end-1] *= pop!(s)
end

# - divide
push!(commands, Command(":/", :c_div!))
function c_div!(s, cov)
  s[end-1] /= pop!(s)
end

# - square root
push!(commands, Command(":sqrt", :c_sqrt!))
function c_sqrt!(s, cov)
  s[end] = sqrt( s[end] )
end

# - invert
push!(commands, Command(":inv", :c_inv!))
function c_inv!(s, cov)
  s[end] = 1 / s[end]
end

# - change sign
push!(commands, Command(":chs", :c_chs!))
function c_chs!(s, cov)
  s[end] = -1 * s[end]
end

# - exponent - power
push!(commands, Command(":exp", :c_exp!))
function c_exp!(s, cov)
  s[end-1] ^= pop!(s)
end

# - duplicate
push!(commands, Command(":dup", :c_dup!))
function c_dup!(s, cov)
  push!(s, s[end])
end

# - reverse x and y
push!(commands, Command(":rev", :c_reverse!))
function c_reverse!(s, cov)
  x = s[end]
  s[end] = s[end-1]
  s[end-1] = x
end

# - modulus
push!(commands, Command(":%", :c_mod!))
function c_mod!(s, cov)
  divisor = pop!(s)
  s[end] = s[end] % divisor
end

# - sine
push!(commands, Command(":sin", :c_sin!))
function c_sin!(s, cov)
  s[end] = sin( s[end] )
end

# - cosine
push!(commands, Command(":cos", :c_cos!))
function c_cos!(s, cov)
  s[end] = cos( s[end] )
end

# - tangent
push!(commands, Command(":tan", :c_tan!))
function c_tan!(s, cov)
  s[end] = tan( s[end] )
end

# - arcsine
push!(commands, Command(":asin", :c_asin!))
function c_asin!(s, cov)
  s[end] = asin( s[end] )
end

# - arccosine
push!(commands, Command(":acos", :c_acos!))
function c_acos!(s, cov)
  s[end] = acos( s[end] )
end

# - arctangent
push!(commands, Command(":atan", :c_atan!))
function c_atan!(s, cov)
  s[end] = atan( s[end] )
end

# - pi
push!(commands, Command(":pi", :c_pi!))
function c_pi!(s, cov)
  push!(s, pi)
end

# - Euler's number
push!(commands, Command(":e", :c_euler!))
function c_euler!(s, cov)
  push!(s, ℯ)
end

# - log 10
push!(commands, Command(":log", :c_log10!))
function c_log10!(s, cov)
  s[end] = log10( s[end] )
end

# - natural log
push!(commands, Command(":ln", :c_ln!))
function c_ln!(s, cov)
  s[end] = log( s[end] )
end

# - degrees to radians
push!(commands, Command(":dtor", :c_dtor!))
function c_dtor!(s, cov)
  s[end] = s[end] * pi / 180
end

# - radians to degrees
push!(commands, Command(":rtod", :c_rtod!))
function c_rtod!(s, cov)
  s[end] = s[end] * 180 / pi
end

# - factorial
push!(commands, Command(":!", :c_factorial!))
function c_factorial!(s, cov)
  s[end] = factorial( Int64(s[end]) )
end

# - absolute value
push!(commands, Command(":abs", :c_abs!))
function c_abs!(s, cov)
  s[end] = abs( s[end] )
end


if length(args) == 0
  push!(args, "help")
end

if  args[1] == "--help" || args[1] == "help"
  println("usage: comp <list>")
  println()
  println("  <list> contains a sequence of operations (commands or values)")
  println()
  println("  for example, 'comp 5 :sqrt 1 :- 2 :/' calculates the golden ratio")
  println()
  println("commands:")
  for c in commands
    print(string(c.symbol), "  ")
  end
  println("\n")
  return
elseif args[1] == "--version" || args[1] == "version"
  println("comp ", comp_version)
else
  main(args)
end
