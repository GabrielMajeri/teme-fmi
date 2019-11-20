d = ['0', '1']
with open('v2p3.txt') as fin:
    m = int(next(fin))
    d += [next(fin).strip() for _ in range(m)]
    s = next(fin).strip()

# min_count[i] = numărul minim de cuvinte în care pot împărți
# subșirul s[i:]

n = len(s)
min_count = [0] * n
min_prev = [-1] * n
min_word = [''] * n

# Ultima cifră, indiferent dacă este 0 sau 1,
# sigur este în dicționar
min_count[-1] = 1
min_word[-1] = s[-1]

for i in range(n - 2, -1, -1):
    min_d = n
    min_w = ''
    min_p = 0
    for word in d:
        k = len(word)

        # Dacă subșirul iese din șirul inițial nu e bun
        if i + k >= n:
            continue

        # Vedem dacă se potrivește
        if s[i:i + k] == word:
            current_d = 1 + min_count[i + k]
            if min_d > current_d:
                min_d = current_d
                min_w = word
                min_p = i + k

    min_count[i] = min_d
    min_word[i] = min_w
    min_prev[i] = min_p

# print(min_count)
# print(min_word)
# print(min_prev)

fragments = []

current = 0
while current != -1:
    fragments.append(min_word[current])
    current = min_prev[current]

print(f'Minim {len(fragments)} șiruri')
print('+'.join(fragments))
