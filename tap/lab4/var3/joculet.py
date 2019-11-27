with open('joculet.in') as fin:
    n = int(next(fin))
    v = [int(x) for x in next(fin).split()]

max_scores = [[0 for _ in range(n)] for _ in range(n)]

for i in range(n):
    max_scores[i][i] = v[i]

for i in range(n - 1):
    max_scores[i][i + 1] = max(v[i], v[i + 1])

for x in range(2, n):
    for y in range(n - x):
        i, j = y, x + y
        choose_i = v[i] + min(max_scores[i + 2][j], max_scores[i + 1][j - 1])
        choose_j = v[j] + min(max_scores[i][j - 2], max_scores[i + 1][j - 1])
        max_scores[i][j] = max(choose_i, choose_j)

with open('joculet.out', 'w') as fout:
    player1_score = max_scores[0][n - 1]
    player2_score = sum(v) - player1_score
    score_diff = player1_score - player2_score

    print(score_diff, file=fout)
