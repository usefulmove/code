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

args = ARGS

@enum Command begin
  CNULL = 0
  CADD = 1
  CSUB = 2
  CMUL = 3
  CDIV = 4
  CSQRT = 5
  CINV = 6
end

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

# list evaluation engine
function evaluate_list(stack_in, oplist)
  println(string("( evaluate_list.stack_in: ", stack_in, " )"))
  println(string("( evaluate_list.oplist: ", oplist, " )"))

  if (length(oplist) == 0)
    return stack_in
  else
    stack_out = stack_in
  end

  for i in 1:length(oplist)
    println(string("( evaluate_list.i: ", i, " )"))
    stack_out = process_node(stack_out, oplist[i])
  end

  return stack_out
end

# operation execution - need two versions:
#  f(double, string) and f(double, double)
function process_node(stack_in, cmd_or_val)
  stack_out = stack_in

  command_id = iscommand(cmd_or_val)

  # pop node off list and update stack
  # based on operation
  if command_id != CNULL # ( command )
    # parse string for command and identify command_id
    # and update stack based on command_id
    stack_out = execute_command(stack_out, command_id)
  else # ( value )
    # add to stack
    push!(stack_out, parse(Float64, cmd_or_val))
  end

  return stack_out
end

# returns command_id given string input
function iscommand(sinput) 
  if sinput == ":+"
    return CADD
  elseif sinput == ":-"
    return CSUB
  elseif sinput == ":x"
    return CMUL
  elseif sinput == ":/"
    return CDIV
  elseif sinput == ":sqrt"
    return CSQRT
  elseif sinput == ":inv"
    return CINV
  else
    return CNULL
  end
end

function execute_command(stack_in, command_id)
  o = stack_in

  println(string("( execute_command.stack_in: ", stack_in, " )"))
  println(string("( execute_command.command_id: ", command_id, " )"))

  if command_id == CADD
    o[end-1] += pop!(o)
  elseif command_id == CSUB
    o[end-1] -= pop!(o)
  elseif command_id == CMUL
    o[end-1] *= pop!(o)
  elseif command_id == CDIV
    o[end-1] /= pop!(o)
  elseif command_id == CSQRT
    o[end] = sqrt( o[end] )
  elseif command_id == CINV
    o[end] = 1 /  o[end]
  #elseif command_id == CMOD
  #  TODO
  end

  return o
end

function main(ops)
  # create stack
  stack = Vector{Float64}(undef, 0) 

  # evaluate list of arguments and update stack
  stack = evaluate_list(stack, ops) 

  # return result of argument list evaluation
  println(
    string(
      string( stack ),
      "\r"
    )
  )
end

main(args)
