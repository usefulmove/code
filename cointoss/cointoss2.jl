#!julia

using Statistics

const TOTAL_TOSSES = 1e7

pattern_a = [false, true, true]
pattern_b = [true, true, false]

tosses = Vector{Bool}()

pattern_a_found = 0
pattern_b_found = 0

for i in 1:TOTAL_TOSSES
  # toss and increment counters
  push!(tosses, rand(Bool))

  # check for patterns
  if tosses == pattern_a
    global pattern_a_found += 1
    global tosses = Vector{Bool}()
  elseif tosses == pattern_b
    global pattern_b_found += 1
    global tosses = Vector{Bool}()
  end

  if length(tosses) == length(pattern_a)
    popfirst!(tosses)
  end
end

println(pattern_a_found / (pattern_a_found + pattern_b_found))
println(pattern_b_found / (pattern_a_found + pattern_b_found))
