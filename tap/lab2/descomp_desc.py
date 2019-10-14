from bisect import bisect
from math import inf

lines = (line.strip() for line in open('descomp_desc.txt'))

n = int(next(lines))
nums = map(int, next(lines).split())

SUP_LIM = inf
INF_LIM = -inf

intervals = [INF_LIM, SUP_LIM]

subintervals = []
tails = []

for value in nums:
    # print('Now processing', value)

    idx = bisect(tails, value)

    # print('Inserting at', idx)

    if idx == len(subintervals):
        subintervals.append([value])
        tails.append(value)
    elif tails[idx] >= value:
        subintervals[idx].append(value)
        tails[idx] = value

print(*subintervals, sep='\n')
