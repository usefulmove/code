#!/usr/local/bin/julia

args = ARGS

function main( prime_n_target )
  count_prime = 1::Int64
  current_prime = 2::Int64

  while ( count_prime < prime_n_target )
    current_prime = find_next_prime( current_prime )
    count_prime += 1
  end

  println( string( current_prime ) )
end

function find_next_prime( start )
  count_walk = start + 1
  count_test = 2::Int64

  while ( count_test < count_walk )
    if count_walk % count_test == 0 # not a prime number
      count_walk += 1 # increment walk counter
      count_test = 2 # reset test counter
    end

    count_test += 1
  end

  return count_walk # prime number
end

main( parse(Int64,args[1]) )
