from itertools import accumulate

ans = list(accumulate(range(8 + 1), lambda acc, a: acc + a * a))[-1]

acc = 0 # initialize accumulator
ans2 = [acc := acc + a * a for a in range(8 + 1)][-1]

print(f"  {ans = }")
print(f"  {ans2 = }")
