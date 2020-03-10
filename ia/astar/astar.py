from collections import defaultdict, namedtuple
from math import inf


# Dicționar în care rețin cât este valoarea dată de euristică fiecărui nod,
# adică `f(nod)`
heuristic = {}

# Dicționar de dicționare în care rețin arcele grafului.
# `arcs[u][v]` reține ponderea arcului (u, v).
arcs = defaultdict(dict)


# Citire date
input_path = 'curs.in'
with open(input_path) as fin:
    # Citesc valorile funcției euristice
    num_nodes = int(next(fin))
    for _ in range(num_nodes):
        label, value = next(fin).split()
        heuristic[label] = float(value)

    # Citesc arcele
    num_arcs = int(next(fin))
    for _ in range(num_arcs):
        u, v, cost = next(fin).split()
        arcs[u][v] = float(cost)

    # Citesc nodul de la care pornesc și nodul scop
    start, target = next(fin).split()


print("Noduri și euristici:")
for (node, heuristic_value) in heuristic.items():
    print(f"- {node} cu valoarea {heuristic_value}")

print("Arce:")
for (u, neighbors) in arcs.items():
    for (v, cost) in neighbors.items():
        print(f"- {u} -> {v} de pondere {cost}")

# Nodurile deschise au fost descoperite dar trebuie explorate
open = [start]
print("Lista open inițială:", open)

# Nodurile închise nu vor mai fi vizitate (dar ar putea fi actualizate)
closed = []
print("Lista closed inițială:", closed)

# Distanța minime găsită de la nodul de start la un anumit nod
dists = defaultdict(lambda: inf)
# Nodul start e la distanță zero de el însuși
dists[start] = 0

# Pentru un nod `x`, `costs[x]` este egal cu `dists[x] + heuristic[x]`.
# Reprezintă cât estimăm că ar fi distanța minimă până la destinație,
# pentru un drum care trece prin acest nod
costs = defaultdict(lambda: inf)
costs[start] = heuristic[start]

# Dicționar care reține predecesorul unui nod
# Inițial niciun nod nu are predecesor
preds = defaultdict(lambda: None)


def is_predecessor(node, pred):
    while node:
        if node == pred:
            return True
        node = preds[node]
    # Dacă ajung la rădăcină
    return False


# Cât timp mai avem de vizitat noduri
while open:
    print("Lista open nu este vidă, mai execut un pas al algoritmului")

    # Extragem un nod din listă
    current = open.pop(0)

    print(f"Extrag nodul '{current}' din open și îl pun în closed")
    closed.append(current)

    # Dacă am ajuns la destinație
    if current == target:
        print("Nodul extras din open este scopul")
        break

    # Pentru a găsi vecinii acestui nod,
    # ne uităm la toate arcele care pleacă din el.
    successors = arcs[current]
    print(f"Succesorii nodului '{current}' sunt", successors)

    for (successor, arc_cost) in successors.items():
        # Dacă acest nod se află în drumul cel mai scurt până la nodul curent,
        # îl sar pentru a evita ciclurile.
        if is_predecessor(current, successor):
            continue

        # Văd ce distanță aș obține dacă aș ajunge la succesor prin nodul curent
        new_distance = dists[current] + arc_cost
        new_cost = new_distance + heuristic[successor]

        if successor in closed:
            # Când actualizăm nodurile din lista closed, ne intersează dacă obținem
            # o euristică (nu neapărat distanță) mai bună.
            if new_cost < costs[successor]:
                print(f"Nodul {successor} se află deja în closed cu "
                    + "o distanță mai mare, așa că îl actualizez")

                preds[successor] = current
                dists[successor] = new_distance
                costs[successor] = new_cost

        elif successor in open:
            # Când actualizăm nodurile din lista open, ne interesează doar dacă
            # obținem o distanță (nu o euristică) mai bună.
            if new_distance < dists[successor]:
                print(f"Nodul {successor} se află deja în open cu "
                    + "o distanță mai mare, așa că îl actualizez")

                preds[successor] = current
                dists[successor] = new_distance
                costs[successor] = new_cost
        else:
            # Când nodul nu este în nicio listă (este prima dată când îl explorăm),
            # este pus automat în lista deschisă.
            open.append(successor)

            preds[successor] = current
            dists[successor] = new_distance
            costs[successor] = new_cost

    print("Lista open după ce am adăugat succesorii este:", open)

    # Sortăm lista crescător după valoarea euristică:
    open.sort(key=lambda node: costs[node])

    print("Lista open după sortare este:", open)
    print("Lista closed:", closed)

    # Adaug un newline ca să se vadă mai bine iterațiile algoritmului
    print()

print("Am ajuns la nodul țintă")

# Reconstruiesc drumul folosindu-mă de predecesori
path = []
while current:
    path.append(current)
    current = preds[current]

path.reverse()

# Afișez drumul și lungimea sa
min_cost = dists[target]
print(f"Un drum de cost minim {min_cost} este:", path)
