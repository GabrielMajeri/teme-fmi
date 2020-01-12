with open('palindrome.txt') as fin:
    word = next(fin)

# pal[i][j] = True if subsequence between i and j is palindrome
#           = s[i] == s[j] and pal[i + 1][j - 1] (stop when i > j)

n = len(word)
pal = [[False for _ in range(n)] for _ in range(n)]

for i in range(n):
    pal[i][i] = True
