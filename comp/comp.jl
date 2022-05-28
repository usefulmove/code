#!/usr/bin/julia

#=
  golden ratio
  % comp 5 sqrt 1 - 2 / eval
  
  time left at full throughput
  % comp 3.2e3 4500 + 1300 / 4 / e[val]
  
  memory assignment
  % comp 3 x =

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

# operation execution
function process_node(cmd_or_val)
  command_id = iscommand(cmd_or_val)

  # pop node off list and update stack
  # based on operation
  if command_id != CNULL # ( command )
    # parse string for command and identify command_id
    # and update stack based on command_id
    execute_command(command_id)
  else # ( value )
    # add to stack
    push!(cstack, parse(Float64, cmd_or_val))
  end
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

function execute_command(command_id)
  debug_print(string("( execute_command.cstack: ", cstack, " )"))
  debug_print(string("( execute_command.command_id: ", command_id, " )"))

  if command_id == CADD
    cstack[end-1] += pop!(cstack)
  elseif command_id == CSUB
    cstack[end-1] -= pop!(cstack)
  elseif command_id == CMUL
    cstack[end-1] *= pop!(cstack)
  elseif command_id == CDIV
    cstack[end-1] /= pop!(cstack)
  elseif command_id == CSQT
    cstack[end] = sqrt( cstack[end] )
  elseif command_id == CINV
    cstack[end] = 1 / cstack[end]
  elseif command_id == CCHS
    cstack[end] = -1 * cstack[end]
  elseif command_id == CPOW
    cstack[end-1] ^= pop!(cstack)
  elseif command_id == CDUP
    push!(cstack, cstack[end])
  elseif command_id == CREV
    x = cstack[end]
    cstack[end] = cstack[end-1]
    cstack[end-1] = x
  elseif command_id == CMOD
    cstack[end-1] = cstack[end-1] % pop!(cstack)
  elseif command_id == CSIN
    cstack[end] = sin( cstack[end] )
  elseif command_id == CCOS
    cstack[end] = cos( cstack[end] )
  elseif command_id == CTAN
    cstack[end] = tan( cstack[end] )
  elseif command_id == CASN
    cstack[end] = asin( cstack[end] )
  elseif command_id == CACN
    cstack[end] = acos( cstack[end] )
  elseif command_id == CATN
    cstack[end] = atan( cstack[end] )
  elseif command_id == CPI
    push!(cstack, pi)
  elseif command_id == CEUL
    push!(cstack, ℯ)
  elseif command_id == CLOG
    cstack[end] = log10( cstack[end] )
  elseif command_id == CNLG
    cstack[end] = log( cstack[end] )
  elseif command_id == CDTR
    cstack[end] = cstack[end] * pi / 180
  elseif command_id == CRTD
    cstack[end] = cstack[end] * 180 / pi
  elseif command_id == CFAC
    cstack[end] = factorial( Int64(cstack[end]) )
  elseif command_id == CABS
    cstack[end] = abs( cstack[end] )
  elseif command_id == CGOL
    push!(cstack, (sqrt(5)-1)/2)
  end
end

function debug_print(message)
  if debug println(message) end
end

function main(oplist)
  # create computation stack
  global cstack = Vector{Float64}(undef, 0) 

  # evaluate list of arguments and update stack by
  # mapping node evaluation to the operations list.
  # the node evaluation function mutates the stack.
  map(process_node, oplist)

  # return result of argument list evaluation
  println(
    string(
      string( cstack ),
      "\r"
    )
  )
end

main(args)
