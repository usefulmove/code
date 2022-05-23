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

# list evaluation engine
function evaluate_list(sin, ops)

  if (length(ops) == 0)
    return sin
  else
    sout = sin
  end

  while length(ops) > 0
    sout = process_node( sout, ops[i] ) # TODO correct this
  end

  return sout
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
    push!(sout, parse(Float64, op))
  end

  return sout
end

# returns command_id given string input
function is_symbol(sinput) 
  if sinput == ":+"
    return CADD
  elseif sinput == ":-"
    return CSUB
  elseif sinput == ":*"
    return CMUL
  elseif sinput == ":/"
    return CDIV
  else
    return CNUL
  end
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
