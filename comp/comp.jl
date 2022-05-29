#!/usr/bin/julia

#=
  golden ratio
  % comp 5 :sqrt 1 :- 2 :/
  
  time left at full throughput
  % comp 3.2e3 4500 + 1300 :/ 4 :/
  
  memory assignment
  % comp 3 :a (?)

=#

# read operations list as argument
args = ARGS

#=
   note: implement linked list as the base
   data structure (stack). atoms on the list
   will either be symbols (commands) or values.
   each calculation is its own linked list of
   operations that processed according to their
   content and order of the list. this is a
   Lisp construct. we will need to build a list
   evaluation engine.

   list structure
   (object - command or value string)
   "32e3"
   "3400"
   "+"
   "2"
   "/"

   the list evaluation engine takes a list
   of strings as an input and returns a 
   value equal to the result of the eval-
   uation.

   <double> = evaluate_list( <string_list> )
=#

function main(oplist)
  # create computation stack
  global cstack = Vector{Float64}(undef, 0) 

  # evaluate list of arguments and update stack by
  # mapping node evaluation to the operations list.
  # the node evaluation function mutates the stack.
  map(process_node!, oplist)

  # return result of argument list evaluation
  println(
    string(
      string( cstack ),
      "\r"
    )
  )
end

struct Command
  symbol::String
  command_f # command function
end

# operation execution
function process_node!(cmd_or_val)
  # execute command function for command or value
  getfield(Main, Symbol(cmdfunction(cmd_or_val)))(cmd_or_val)
end

# command function retrieval method
function cmdfunction(sinput)
  for c in commands
    if c.symbol == sinput
      return c.command_f
    end
  end

  # if not symbols match return stack function
  # to add value to stack
  return :c_addtostack
end

function c_addtostack(s)
  push!(cstack, parse(Float64, s))
end

# build command vector
commands = Vector{Command}(undef, 0) 

# ( add )
push!(commands, Command(":+", :c_add))
function c_add(s)
  cstack[end-1] += pop!(cstack)
end

# ( subtract )
push!(commands, Command(":-", :c_subtract))
function c_subtract(s)
  cstack[end-1] -= pop!(cstack)
end

# ( multiply )
push!(commands, Command(":x", :c_multiply))
function c_multiply(s)
  cstack[end-1] *= pop!(cstack)
end

# ( divide )
push!(commands, Command(":/", :c_divide))
function c_divide(s)
  cstack[end-1] /= pop!(cstack)
end

# ( square root )
push!(commands, Command(":sqrt", :c_squareroot))
function c_squareroot(s)
  cstack[end] = sqrt( cstack[end] )
end

# ( invert )
push!(commands, Command(":inv", :c_invert))
function c_invert(s)
  cstack[end] = 1 / cstack[end]
end

# ( change sign ) 
push!(commands, Command(":chs", :c_changesign))
function c_changesign(s)
  cstack[end] = -1 * cstack[end]
end

# ( exponent - power ) 
push!(commands, Command(":exp", :c_exponent))
function c_exponent(s)
  cstack[end-1] ^= pop!(cstack)
end

# ( duplicate ) 
push!(commands, Command(":dup", :c_duplicate))
function c_duplicate(s)
  push!(cstack, cstack[end])
end

# ( reverse x and y ) 
push!(commands, Command(":rev", :c_reverse))
function c_reverse(s)
  x = cstack[end]
  cstack[end] = cstack[end-1]
  cstack[end-1] = x
end

# ( modulus )
push!(commands, Command(":%", :c_modulus))
function c_modulus(s)
  divisor = pop!(cstack)
  cstack[end] = cstack[end] % divisor
end

# ( sine )
push!(commands, Command(":sin", :c_sine))
function c_sine(s)
  cstack[end] = sin( cstack[end] )
end

# ( cosine )
push!(commands, Command(":cos", :c_cosine))
function c_cosine(s)
  cstack[end] = cos( cstack[end] )
end

# ( tangent )
push!(commands, Command(":tan", :c_tangent))
function c_tangent(s)
  cstack[end] = tan( cstack[end] )
end

# ( arcsine )
push!(commands, Command(":asin", :c_arcsine))
function c_arcsine(s)
  cstack[end] = asin( cstack[end] )
end

# ( arccosine )
push!(commands, Command(":acos", :c_arccosine))
function c_arccosine(s)
  cstack[end] = acos( cstack[end] )
end

# ( arctangent )
push!(commands, Command(":atan", :c_arctangent))
function c_arctangent(s)
  cstack[end] = atan( cstack[end] )
end

# ( pi )
push!(commands, Command(":pi", :c_pi))
function c_pi(s)
  push!(cstack, pi)
end

# ( Euler's number )
push!(commands, Command(":e", :c_euler))
function c_euler(s)
  push!(cstack, ℯ)
end

# ( log 10 )
push!(commands, Command(":log", :c_log10))
function c_log10(s)
  cstack[end] = log10( cstack[end] )
end

# ( natural log )
push!(commands, Command(":ln", :c_ln))
function c_ln(s)
  cstack[end] = log( cstack[end] )
end

# ( degrees to radians )
push!(commands, Command(":dtor", :c_degreestoradians))
function c_degreestoradians(s)
  cstack[end] = cstack[end] * pi / 180
end

# ( radians to degrees )
push!(commands, Command(":rtod", :c_radianstodegrees))
function c_radianstodegrees(s)
  cstack[end] = cstack[end] * 180 / pi
end

# ( factorial )
push!(commands, Command(":!", :c_factorial))
function c_factorial(s)
  cstack[end] = factorial( Int64(cstack[end]) )
end

# ( absolute value )
push!(commands, Command(":abs", :c_absolutevalue))
function c_absolutevalue(s)
  cstack[end] = abs( cstack[end] )
end

# ( golden ratio )
push!(commands, Command(":gold", :c_goldenratio))
function c_goldenratio(s)
  push!(cstack, (sqrt(5)-1)/2)
end


if args[1] == "--help"
  println("usage:  comp 5 :sqrt 1 :- 2 :/")
  println()
  println("commands:")
  for c in commands
    println("  ", string( c.symbol ))
  end
  return
else
  main(args)
end
