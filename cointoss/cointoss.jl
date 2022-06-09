#!julia

using Statistics

const TOTAL_TOSSES = 1e9

pattern_a = [false, true, true]
pattern_b = [true, false, true]

tosses = Vector{Bool}()


pattern_a_counter = 0
pattern_b_counter = 0

pattern_a_found = Vector{Int64}()
pattern_b_found = Vector{Int64}()

for i in 1:TOTAL_TOSSES
  # toss and increment counters
  push!(tosses, rand(Bool))
  global pattern_a_counter += 1
  global pattern_b_counter += 1

  # check for pattern
  if tosses == pattern_a && pattern_a_counter >= length(pattern_a)
    push!(pattern_a_found, pattern_a_counter)
    pattern_a_counter = 0 # reset counter
  elseif tosses == pattern_b && pattern_b_counter >= length(pattern_b)
    push!(pattern_b_found, pattern_b_counter)
    pattern_b_counter = 0 # reset counter
  end

  if length(tosses) == length(pattern_a)
    popfirst!(tosses)
  end
end

println(mean(pattern_a_found))
println(mean(pattern_b_found))
