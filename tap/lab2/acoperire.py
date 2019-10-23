"""
Soluția greedy:
    Sortăm intervalele după timpul de început.

    Fie [a, b] intervalul curent pe care încercăm să-l acoperim.

    Mergem în ordine prin intervale, și alegem intervalul care
    începe înainte de „a” și acoperă cel mai mult (se termină cel mai târziu).

    După ce l-am ales, îl punem în soluție, capătul lui din dreapta devine
    noul „a”, și continuăm algoritmul.

Demonstrație corectitudine:
    Notăm cu [A, B] intervalul curent pe care vrem să-l acoperim.

    1) Soluția construită de greedy este corectă:
    Întotdeauna alegem un interval dintre cele care încep la stânga lui A,
    deci nu formăm o soluție invalidă.

    Deasemenea, parcurgem liniar toate intervalele, nu putem omite vreunul
    care ar apărea într-o soluție.

    2) Presupunem că soluția greedy nu ar fi optimă.
    Să zicem că (x, y) ar fi primul interval ales de greedy care diferă,
    și (x', y') ar fi intervalul ales de soluția optimă.

    Fie A capătul curent din stânga al intervalului pe care vrem să-l acoperim.
    Avem că:
        x  < A < y  (pentru că soluția greedy este corectă)
        x' < A < y' (pentru că soluția optimă este corectă)
        y' <= y     (pentru că la greedy alegem intervalul
                     care să aibă y maxim)

    Deoarece y' <= y, indiferent ce alte intervale mai alege soluția optimă mai încolo,
    putem să înlocuim intervalul [x', y'] cu [x, y] și obținem că soluția greedy
    este mai bună decât cea optimă, contradicție.
"""

fin = open('acoperire.txt')

lines = map(str.strip, fin)

a, b = map(int, next(lines).split())

n = int(next(lines))
intervals = [tuple(map(int, next(lines).split())) for _ in range(n)]

intervals.sort()

# print('Intervalele sortate:', *intervals)


def find_next_interval(start):
    current = start
    max_index = current

    while current < len(intervals) and intervals[current][0] <= a:
        if intervals[current][1] > intervals[max_index][1]:
            max_index = current

        current += 1

    if current < len(intervals) and intervals[max_index][0] > a:
        return -1

    return max_index


solution = []

ok = True
index = 0
while ok and a < b:
    index = find_next_interval(index)

    if index == -1:
        ok = False
        break

    a = intervals[index][1]
    solution.append(intervals[index])
    index += 1

if ok:
    print(*solution)
else:
    print('Nu există soluție')
