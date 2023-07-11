def solve_2x2(solutions, square, row_sums, col_sums, row=0, col=0):
    # Base case: if all cells are filled
    if row == 2:
        # Check if all column sums are satisfied
        if all(sum(square[r][c] for r in range(2)) == col_sums[c] for c in range(2)):
            # If they are, add the solution to the list of solutions
            solutions.append([row.copy() for row in square])
        return

    # Calculate the next cell
    next_row, next_col = (row, col+1) if col < 1 else (row+1, 0)

    # Try all possible numbers for this cell
    for num in range(1, 10):  # from 1 to 9
        # Check if this number satisfies the row sum
        if sum(square[row][:col]) + num <= row_sums[row]:
            # If it does, put it in the cell
            square[row][col] = num
            # And move on to the next cell
            solve_2x2(solutions, square, row_sums, col_sums, next_row, next_col)
            # Remove the number from the cell (backtrack)
            square[row][col] = None


# Initialize the square with None values
square = [[None, None], [None, None]]

# Initialize the list of solutions
solutions = []

# Run the function
solve_2x2(solutions, square, [5, 9], [6, 8])

# Print all solutions
for solution in solutions:
    for row in solution:
        print(row)
    print()
