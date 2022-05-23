#!/usr/bin/julia

#=
  golden ratio
  % op 5 sqrt 1 - 2 / eval
  
  time left at full throughput
  % op 3.2e3 4500 + 1300 / 4 / e[val]
  
  memory assignment
  % op 3 x =

  read argument list
  args = ARGS

=#

op = ARGS

#=

  note: implement linked list as the base
  data structure (stack). atoms on the list
  will either be symbols or values. each
  calculation is its own linked list of op-
  erations that processed according to
  their content and order of the list. this
  is Lisp construct. i will need to build
  a list evaluation engine.

  list structure
  (object - symbol or value string)
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

# list evaluation engine
function evaluate_list(String[], op_list)
  register = 0.0::Float64
  stack = Vector{Float64}(undef, 32)

  if (length(op_list) == 0)
    return TODO
  end

  for i in length(op_list)
    # pop node off list and update stack
    # and register based on operation
    register = process_node( op_list[i] ) 
  end

  return register
end

# operation execution - need two versions:
#  f(double, string) and f(double, double)
function process_node(current_register, op)
  if TODO # value
    # update stack
    value = parse(Float64, op)
  else # symbol
    # TODO parse string for symbol and identify id
    # update register and stack
    register = execute_command(register, symbol_id)
  end
end


# return result of argument list evaluation
println(
  string(
    string( evaluate_list(op) ),
    "\r"
  )
)
