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

debug = false

args = ARGS

@enum Command begin
  CNULL # no op
  CADD  # add
  CSUB  # subtract
  CMUL  # multiply
  CDIV  # divide
  CSQT  # squart root
  CINV  # invert (1/x)
  CCHS  # change sign
  CPOW  # power
  CDUP  # duplicate
  CREV  # reverse x and y
  CMOD  # modulus
  CSIN  # sine
  CCOS  # cosine
  CTAN  # tangent
  CASN  # arcsine
  CACN  # arccosine
  CATN  # arctangent
  CPI   # pi
  CEUL  # e
  CLOG  # log 10
  CNLG  # natural log
  CDTR  # degrees to radians
  CRTD  # radians to degrees
  CFAC  # factorial
  CABS  # absolute value
  CGOL  # golden ratio
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
  debug_print(string("( evaluate_list.stack_in: ", stack_in, " )"))
  debug_print(string("( evaluate_list.oplist: ", oplist, " )"))

  if (length(oplist) == 0)
    return stack_in
  else
    stack_out = stack_in
  end

  for i in 1:length(oplist)
    debug_print(string("( evaluate_list.i: ", i, " )"))
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
    return CSQT
  elseif sinput == ":inv"
    return CINV
  elseif sinput == ":chs"
    return CCHS
  elseif (sinput == ":pow") | (sinput == ":exp")
    return CPOW
  elseif sinput == ":dup" 
    return CDUP
  elseif sinput == ":rev"
    return CREV
  elseif sinput == ":%"
    return CMOD
  elseif sinput == ":sin"
    return CSIN
  elseif sinput == ":cos"
    return CCOS
  elseif sinput == ":tan"
    return CTAN
  elseif sinput == ":asin"
    return CASN
  elseif sinput == ":acos"
    return CACN
  elseif sinput == ":atan"
    return CATN
  elseif sinput == ":pi"
    return CPI
  elseif sinput == ":e"
    return CEUL
  elseif sinput == ":log"
    return CLOG
  elseif sinput == ":ln"
    return CNLG
  elseif sinput == ":dtor"
    return CDTR
  elseif sinput == ":rtod"
    return CRTD
  elseif sinput == ":!"
    return CFAC
  elseif sinput == ":abs"
    return CABS
  elseif sinput == ":gold"
    return CGOL
  else
    return CNULL
  end
end

function execute_command(stack_in, command_id)
  o = stack_in

  debug_print(string("( execute_command.stack_in: ", stack_in, " )"))
  debug_print(string("( execute_command.command_id: ", command_id, " )"))

  if command_id == CADD
    o[end-1] += pop!(o)
  elseif command_id == CSUB
    o[end-1] -= pop!(o)
  elseif command_id == CMUL
    o[end-1] *= pop!(o)
  elseif command_id == CDIV
    o[end-1] /= pop!(o)
  elseif command_id == CSQT
    o[end] = sqrt( o[end] )
  elseif command_id == CINV
    o[end] = 1 / o[end]
  elseif command_id == CCHS
    o[end] = -1 * o[end]
  elseif command_id == CPOW
    o[end-1] ^= pop!(o)
  elseif command_id == CDUP
    push!(o, o[end])
  elseif command_id == CREV
    x = o[end]
    o[end] = o[end-1]
    o[end-1] = x
  elseif command_id == CMOD
    o[end-1] = o[end-1] % pop!(o)
  elseif command_id == CSIN
    o[end] = sin( o[end] )
  elseif command_id == CCOS
    o[end] = cos( o[end] )
  elseif command_id == CTAN
    o[end] = tan( o[end] )
  elseif command_id == CASN
    o[end] = asin( o[end] )
  elseif command_id == CACN
    o[end] = acos( o[end] )
  elseif command_id == CATN
    o[end] = atan( o[end] )
  elseif command_id == CPI
    push!(o, pi)
  elseif command_id == CEUL
    push!(o, ℯ)
  elseif command_id == CLOG
    o[end] = log10( o[end] )
  elseif command_id == CNLG
    o[end] = log( o[end] )
  elseif command_id == CDTR
    o[end] = o[end] * pi / 180
  elseif command_id == CRTD
    o[end] = o[end] * 180 / pi
  elseif command_id == CFAC
    o[end] = factorial( Int64(o[end]) )
  elseif command_id == CABS
    o[end] = abs( o[end] )
  elseif command_id == CGOL
    push!(o, (sqrt(5)-1)/2)
  end

  return o
end

function debug_print(message)
  if debug println(message) end
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
