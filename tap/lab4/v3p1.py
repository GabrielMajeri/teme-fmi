with open('v3p1.txt') as fin:
    words = list(next(fin).split())

# We build a look-up table where we index by the last two characters of the word,
# and return the length of the longest chain ending in them (as well as a word
# ending with them so we can rebuild the chain).
max_seqs = {}
for word in words:
    firsts, lasts = word[:2], word[-2:]

    new_len = 1
    if firsts in max_seqs:
        new_len = max_seqs[firsts][0] + 1

    prev_len = max_seqs.get(lasts, (0, ''))[0]

    if new_len > prev_len:
        max_seqs[lasts] = (new_len, word)

# Now that we built the look-up table we need to go back through the values
# and find the longest chain
max_len, max_word = max(max_seqs.values(), key=lambda p: p[0])

stack = [max_word]
while max_len > 1:
    firsts = max_word[:2]
    max_len, max_word = max_seqs[firsts]
    stack.append(max_word)

print(*reversed(stack))
