"""
Aceleași date de intrare ca la cealaltă problemă, dar acum fiecare activitate
are un profit și trebuie să aleg o submulțime de profit maxim.

Rezolvare:
    La fel ca la greedy, sortăm după capătul din dreapta, apoi la fiecare pas
    încercăm să adăugăm un nou interval: comparăm profitul pe care îl obținem
    fără el, cu profitul pe care l-am avea dacă îl adăugăm
    (și în acest caz am scoate intervalele incompatibile cu el).
"""

from collections import namedtuple


Activity = namedtuple("Activity", ("start", "finish", "profit"))

# Citire date
activities = []
with open("actprof.in") as fin:
    n = int(next(fin))
    for _ in range(n):
        s, f, p = map(int, next(fin).split())
        activities.append(Activity(s, f, p))

activities.sort(key=lambda act: act.finish)

solution = [activities[0]]
profit = activities[0].profit

# Funcție care caută binar unde am putea insera intervalul
def lower_bound(left, right, start):
    diff = right - left
    if diff == 1:
        if solution[left].finish <= start:
            return right
        else:
            return left

    mid = left + diff // 2

    if solution[mid].finish <= start:
        return lower_bound(mid, right, start)
    else:
        return lower_bound(left, mid, start)


# Adaugă incremental intervalele
for i in range(1, n):
    activity = activities[i]
    idx = lower_bound(0, len(solution), activity.start)

    # Trebuie să determinăm dacă adăugarea acestei activități ne-ar crește profitul sau nu
    extra_profit = activity.profit

    # Dacă nu inserăm la final, trebuie să vedem dacă suprascriem alt interval
    overlap_left = False
    if idx < len(solution) and solution[idx].finish > activity.start:
        extra_profit -= solution[idx].profit
        overlap_left = True

    overlap_right = False
    if idx + 1 < len(solution) and solution[idx + 1].start < activity.finish:
        extra_profit -= solution[idx + 1].profit
        overlap_right = True

    # Dacă se merită să-l adăugăm
    if extra_profit > 0:
        # Eliminăm incompatibilitățile
        if overlap_left:
            del solution[idx]
        if overlap_right:
            del solution[idx + 1]

        solution.insert(idx, activity)
        profit += extra_profit

# Afișează soluția
print(profit)
for activity in solution:
    print(activity.start, activity.finish)
