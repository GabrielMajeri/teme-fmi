"""
Algoritm greedy:

Soluția teoretică:
Cât timp mai am muchii nevizitate:
    Aleg o frunză v.
    Fie (u, v) o muchie adiacentă.
    Marchez vecinul u ca vizitat.
    Elimin nodurile u/v pentru că sunt vizitate.

Justificare:
    Primul pas este corect:
    Arătăm că există I mulțime independentă de noduri
    cu număr maxim de noduri care conține frunza v, aleasă de greedy.

    Fie I optimă (cu nr. maxim de noduri).
    1) Frunza v aparține lui I => OK
    2) Frunza v nu aparține lui I:
        a) u aparține lui I
            => îl scoatem pe u din I și îl înlocuim cu v, I tot optim și independent rămâne.
        b) u nu aparține lui I
            => îl putem pune pe v, și am obține o soluție mai bună decât I, contradicție.

Soluția practică (cu o parcurgere):
DF:
    for (fiu nevizitat)
        DF(fiu)

    if (am parcurs toți fii și nu i-am marcat)
        marchează acest nod
        return marcat
    else
        return nu marcăm
"""

fin = open('arbore.txt')

lines = map(str.strip, fin)

n, m = map(int, next(lines).split())

neighbors = {i: [] for i in range(n)}
for _ in range(m):
    a, b = map(int, next(lines).split())
    a -= 1
    b -= 1
    neighbors[a].append(b)
    neighbors[b].append(a)

#print(neighbors)

visited = [False] * n
marked = []


def dfs(node):
    visited[node] = True

    marked_children = False
    for neighbor in neighbors[node]:
        if visited[neighbor]:
            continue

        if dfs(neighbor):
            marked_children = True

    if marked_children:
        return False
    else:
        marked.append(node)
        return True


dfs(0)

solution = [node + 1 for node in marked]

print(len(solution))
print(*solution)
