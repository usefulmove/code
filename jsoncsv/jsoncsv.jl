#!julia

using JSON
using CSV
using DataFrames

# read in argument vector
arg = ARGS

# read JSON file into DataFrame
function readjsondf(file)
  jsonstr = read(file, String)
  a = JSON.Parser.parse(jsonstr)
  return reduce(vcat, DataFrame.(a))
end

df = readjsondf(arg[1])

println(string(df))
