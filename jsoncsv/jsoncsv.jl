#!julia

using JSON
using CSV
using DataFrames

# read in argument vector
arg = ARGS

println("converting $(arg[1]) to $(arg[2])")

# read JSON file into DataFrame
function readjsondf(file)
  jsonstr = read(file, String)
  a = JSON.Parser.parse(jsonstr)
  return reduce(vcat, DataFrame.(a))
end

df = readjsondf(arg[1])

# write DataFrame to CSV
CSV.write(arg[2], df)
