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

@enum Command begin
  CNUL = 0
  CADD = 1
  CSUB = 2
  CMUL = 3
  CDIV = 4
end

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

stack = Vector{Float64}(undef, 0)

# list evaluation engine
function evaluate_list(Float64[]::i_stack, String[]::ops)

  if (length(ops) == 0)
    return input_stack
  else
    o_stack = i_stack
  end

  while length(ops) > 0
    o_stack = process_node( o_stack, ops[i] ) 
  end

  return o_stack
end

# operation execution - need two versions:
#  f(double, string) and f(double, double)
function process_node(sin, op)
  sout = sin

  symbol_id = is_symbol(op)

  # pop node off list and update stack
  # based on operation
  if symbol_id != CNUL # ( symbol )
    # parse string for symbol and identify id
    # update stack based on symbol_id
    sout = execute_command(sout, symbol_id)
  else # ( value )
    # add to stack
    push!(sout, parse(Float64, op)
  end

  return sout
end

function is_symbol(str) # returns command_id
  if str == ":+"
    return CADD
  elsif str == ":-"
    return CSUB
  elsif str == ":*"
    return CMUL
  elsif str == ":/"
    return CDIV
  else
    return CNUL
end

function execute_command(sin, command_id)
  sout = sin

  if command_id == CADD
    sout[length(sout)-1] += pop!(sout)
  elseif command_id == CSUB
    sout[length(sout)-1] -= pop!(sout)
  elseif command_id == CMUL
    sout[length(sout)-1] *= pop!(sout)
  elseif command_id == CDIV
    sout[length(sout)-1] /= pop!(sout)
  #elseif command_id == CMOD
  #  TODO
  end

  return sout
end


# return result of argument list evaluation
println(
  string(
    string( evaluate_list(op) ),
    "\r"
  )
)
