"""
Se dă o mulțime de N activități, cu timpul lor de început și de sfârșit.

Să se aleagă o submulțime de activități compatibile de cardinal maxim.

Rezolvare:
  cu metoda greedy, sortez după timpul de sfârșit al activităților,
  apoi parcurg liniar și iau câte activități pot.
"""

from collections import namedtuple


# Declar un tip de date custom ca să fie mai ușor să accesez câmpurile
Activity = namedtuple("Activity", ("start", "finish"))

# Citesc datele
activities = []
with open("act.in") as fin:
    n = int(next(fin))
    for _ in range(n):
        s, f = map(int, next(fin).split())
        activities.append(Activity(s, f))

# Le sortez crescător după timpul de sfârșit
activities.sort(key=lambda act: act.finish)

solution = [activities[0]]

for activity in activities[1:]:
    if activity.start >= solution[-1].finish:
        solution.append(activity)

# Afișare soluții
for activity in solution:
    print(activity.start, activity.finish)
