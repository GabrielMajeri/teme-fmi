n = 5
k = 3

max_k = (n * (n - 1)) // 2
if k > max_k:
    print('Error')
    exit(1)

num_inv = [[0 for _ in range(k)] for _ in range(n)]

for k in range(n):
    num_inv[k][0] = 1

for i in range(1, n):
    for j in range(k):
        num_inv[i][j] = sum(num_inv[i - 1][x] for x in range(j))
