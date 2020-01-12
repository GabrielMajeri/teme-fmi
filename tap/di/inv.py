"""
Să se găsească numărul de inversiuni semnificative dintr-o permutare.

Inversiune semnificativă:
    i < j cu a_i > 2 * a_j

7 > 2 * 3  OK
7 > 2 * 4  NU

(7, 3) inversiune (nesemnificativa)
(7, 4) inversiune semnificativa

7 10 5 6 3 4 2 8

Soluție: la fel ca algoritmul de numărat inversiuni din curs,
cel bazat pe interclasare. Numai că acum trebuie făcute două interclasări,
odată ca să le sortăm și o dată ca să numărăm inversiunile.
"""
