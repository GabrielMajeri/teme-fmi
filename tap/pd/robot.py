# Read input data
with open('robot.txt') as fin:
    n, m = map(int, next(fin).split())

    mat = [[int(x) for x in next(fin).split()] for _ in range(n)]

# Initialize vectors and matrices for dynamic programming
sum_max = [[0 for _ in range(m)] for _ in range(n)]
preds = [[(-1, -1) for _ in range(m)] for _ in range(n)]

# The robot starts in the upper left corner
sum_max[0][0] = mat[0][0]
preds[0][0] = (-1, -1)

for i in range(n):
    for j in range(m):
        up = sum_max[i - 1][j]
        left = sum_max[i][j - 1]
        if up > left:
            sum_max[i][j] = mat[i][j] + up
            preds[i][j] = (i - 1, j)
        else:
            sum_max[i][j] = mat[i][j] + left
            preds[i][j] = (i, j - 1)

# Print the solution
print(sum_max[-1][-1])

current = (n - 1, m - 1)
stack = []

while current != (0, 0):
    stack.append(current)
    current = preds[current[0]][current[1]]

stack.append(current)

print(*reversed(stack), sep='\n')
