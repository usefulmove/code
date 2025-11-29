# higher_order_functions.rb

# --- 1. Using a built-in higher-order function: `map` ---
# `map` is a method on arrays that takes a block.
# It iterates over each element of the array, and applies the block to it.
# The result is a new array containing the return values of the block.

puts "--- Example 1: Using `map` ---"
numbers = [1, 2, 3, 4]
squared_numbers = numbers.map do |n|
  n * n
end

puts "Original numbers: #{numbers.inspect}"
puts "Squared numbers: #{squared_numbers.inspect}"
puts "\n"


# --- 2. Creating our own higher-order function ---
# This method takes an argument and a block.
# It calls the block with the argument.
def do_something_with_number(number, &block)
  puts "Calling the block with #{number}..."
  block.call(number)
end

puts "--- Example 2: A custom higher-order function ---"
do_something_with_number(5) do |n|
  puts "The block was called! The number is #{n}."
end
puts "\n"


# --- 3. A function that returns a function (using a lambda) ---
# This method returns a lambda (a type of Proc) that multiplies a number by a given factor.
def multiplier(factor)
  lambda { |n| n * factor }
end

puts "--- Example 3: A function that returns a function ---"
doubler = multiplier(2)
tripler = multiplier(3)

puts "Using the doubler lambda on 5: #{doubler.call(5)}"
puts "Using the tripler lambda on 5: #{tripler.call(5)}"
puts "\n"


# --- 4. Passing a named function (a lambda) to another function ---
# We can store a block in a variable (as a lambda) and pass it to a method.
puts "--- Example 4: Passing a named function ---"
upcaser = lambda { |s| s.upcase }

words = ["hello", "world"]
upcased_words = words.map(&upcaser) # The `&` converts the lambda back into a block

puts "Original words: #{words.inspect}"
puts "Upcased words: #{upcased_words.inspect}"
