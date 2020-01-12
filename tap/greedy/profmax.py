lines = map(str.strip, open('profmax.txt'))

n, m = map(int, next(lines).split())

profits = [None] * m
deadlines = [None] * m

for i in range(m):
    a, b = map(int, next(lines).split())
    profits[i] = a
    deadlines[i] = b

# Sort decreasing based on profits
sorted_activities = sorted(zip(profits, deadlines, range(m)), reverse=True)

fathers = list(range(n + 1))
leftmost = list(range(-1, n))
sizes = [1] * n


def find_father(idx):
    if fathers[idx] == idx:
        return idx

    # Path compression
    father = find_father(fathers[idx])
    fathers[idx] = father
    return father

def union(a, b):
    if sizes[a] < sizes[b]:
        fathers[a] = b
        sizes[b] += sizes[a]
    else:
        fathers[b] = a
        sizes[a] += sizes[b]
    leftmost[b] = min(a, b)

def schedule():
    pass


plan = [None] * n

print(sorted_activities)

for profit, deadline, idx in sorted_activities:
    print(f'PLANING ACTIVITY #{idx}')
    father = find_father(deadline)
    print('FIND FATHER', deadline)
    empty_position = leftmost[father]

    union(empty_position, father)

    plan[empty_position] = idx

    if plan[empty_position - 1]:
        left_neighbor = find_father(empty_position - 1)
        union(left_neighbor, father)


print(fathers)
print(sizes)
print(plan)

"""
Planificam 2, 3
aux[3] = 2
union(aux[3], 3)

Acum: t[2] = 3, aux[3] = 1

Find tată de 2
O să fie 3
aux[3] = 1
UNION(1, 3)

"""
