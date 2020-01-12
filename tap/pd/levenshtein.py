with open('levenshtein.txt') as fin:
    a = next(fin).strip()
    b = next(fin).strip()
    c1, c2, c3 = (int(next(fin)) for _ in range(3))

n = len(a)
m = len(b)

mat = [[0 for _ in range(m)] for _ in range(n)]
preds = [[(0, 0) for _ in range(m)] for _ in range(n)]

for j in range(m):
    mat[0][j] = j * c1

for i in range(1, n):
    mat[i][0] = i * c2
    for j in range(1, m):
        if a[i] == b[j]:
            mat[i][j] = mat[i - 1][j - 1]
        else:
            insert_cost = mat[i][j - 1] + c1
            delete_cost = mat[i - 1][j] + c2
            change_cost = mat[i - 1][j - 1] + c3

            mat[i][j] = min(insert_cost, delete_cost, change_cost)

k = mat[-1][-1]
print('Min dist:', k)

i = n - 1
j = m - 1
k -= 1

stack = []

while k >= 0 and i >= 0 and j >= 0:
    if a[i] == b[j]:
        stack.append(f'păstrăm {a[i]}')
        i -= 1
        j -= 1
        continue

    if i > 0 and j > 0:
        if mat[i - 1][j - 1] + c3 == mat[i][j]:
            stack.append(f'schimbăm {a[i]} cu {b[j]}')
            i -= 1
            j -= 1
            k -= c3
            continue

    if i > 0:
        if mat[i - 1][j] + c2 == mat[i][j]:
            stack.append(f'ștergem {a[i]}')
            i -= 1
            k -= c2
            continue

    if j > 0:
        if mat[i][j - 1] + c1 == mat[i][j]:
            stack.append(f'inserăm {b[j]}')
            j -= 1
            k -= c1
            continue

print('Operations:')
print(*reversed(stack), sep='\n')
