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

stack = Vector{Float64}(undef, 32)

# list evaluation engine
function evaluate_list(Float64[]::current_stack, String[]::ops)

  if (length(ops) == 0)
    return current_staack
  else
    updated_stack = current_stack
  end

  while length(ops) > 0
    updated_stack = process_node( updated_stack, ops[i] ) 
  end

  return updated_stack
end

# operation execution - need two versions:
#  f(double, string) and f(double, double)
function process_node(current_stack, op)
  # pop node off list and update stack
  # based on operation
  if TODO # value
    # add to stack
    value = parse(Float64, op)
    TODO
  else # symbol
    # parse string for symbol and identify id
    # update stack based on symbol_id
    updated_stack = execute_command(updated_stack, symbol_id)
  end

  return updated_stack
end

function execute_command(current_stack, command_id)
  updated_stack = current_stack

  if command_id == CADD
    TODO
  elseif command_id == CSUB
    TODO
  elseif command_id == CMUL
    TODO
  elseif command_id == CDIV
    TODO
  elseif command_id == CMOD
    TODO
  end

  return updated_stack
end


# return result of argument list evaluation
println(
  string(
    string( evaluate_list(op) ),
    "\r"
  )
)
