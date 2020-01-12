"""
Primim numere și ponderile lor asociate.
Practic, acestea definesc o histogramă.

Fiecare pondere este în intervalul [0, 1]
Suma ponderilor este 1.

Pe exemplul:
v = 5 1 3 2 9 6 11
w = 0.1 0.12 0.05 0.1 0.2 0.13 0.3
Avem 6 ca mediană ponderată.

Putem rezolva în O(n log n):
    sortăm vectorul, și apoi facem sumele parțiale pentru ponderi
    până ce în stânga avem < 0.5, și în dreapta <= 0.5

Putem rezolva în O(n) cu ideea de la algoritmul de mediană bazat
pe QuickSort:
    - luăm primul element ca pivot și partiționăm vectorul
    - verificăm dacă este mediană ponderată
        - dacă nu este mediană ponderată mergem recursiv în
          jumătatea în care avem ponderi mai mari
        - în apelul recursiv se păstrează suma extra din stânga/dreapta

Complexitate: O(n), deoarece când fac sumele de ponderi trebuie
să parcurg tot vectorul, chiar dacă la fiecare apel înjumătățesc problema.
"""


def partition(v):
    """Partitions a given array using the first element as the pivot."""
    pivot = v[0]

    smaller = [elem for elem in v[1:] if elem < pivot]
    larger = [elem for elem in v[1:] if elem >= pivot]

    return len(smaller), smaller + [pivot] + larger


def extract_weights(pairs):
    """Extracts the weight component of an array of pairs"""
    return (weight for _, weight in pairs)


def find_median(pairs, left_acc, right_acc):
    pivot_idx, part = partition(pairs)
    pivot_num, pivot_w = pairs[pivot_idx]

    left_sum = left_acc + sum(extract_weights(part[:pivot_idx]))
    right_sum = right_acc + sum(extract_weights(part[pivot_idx + 1:]))

    if left_sum < 0.5:
        if right_sum <= 0.5:
            return pivot_num
        else:
            return find_median(part[pivot_idx + 1:], left_sum + pivot_w, 0)
    else:
        return find_median(part[:pivot_idx], 0, right_sum + pivot_w)


fin = open('medpond.txt')

n = int(next(fin))
nums = [int(x) for x in next(fin).split()]
weights = [float(x) for x in next(fin).split()]

values = list(zip(nums, weights))

print(find_median(values, 0, 0))
