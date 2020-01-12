"""
Se dau n cuvinte.

Fie L dimensiunea unei linii. Să se scrie cuvinte pe linii
astfel încât să se minimizeze
    sumă de (L - O[i])^2 pentru i de la 1 la p.

Trebuie să se păstreze ordinea cuvintelor în text.

Exemplu:
    aa  abc  ab  xyzt  xy  xyz
     2   3    2    4    2   3

L = 6
  aa   4
  abc  3
  ab   4
  xy  xyz

O[1] = 2
O[2] = 2
O[3] = 2
O[4] = 6

cost = 4^2 + 3^2 + 4^2 + 0^2


  aa abc
  ab   4
  xyzt 2
  xy xyz

cost = 4^2 + 2^2

l[i][j] = costul pentru scrierea
          cuvintelor de la i la j
          pe o singură linie

sl[i][j] = spații pentru scrierea
           cuvintelor de la i la j
           pe o singură linie

l[i][i] = (L - len(w[i]))^2
sl[i][i] = L - len(w[i])

sl[i][j] = {
        sl[i][j - 1] - 1 - len(w[j]) dacă >= 0 (adică cuvintele încap)
        infinit, dacă nu încap cuvintele pe aceeași linie
    }

D[j] = costul scrierii cuvintelor de    la 1 la j

D[0] = 0
D[1] = l[1][1]

D[j] = min după 1 <= i <= j din {
        D[i - 1] + l[i][j]
    }

L = 8

sl = 6 2 ∞ ∞ ∞ ∞
       5 2 ∞ ∞ ∞
         6 1 ∞ ∞
           4 1 ∞
             6 2
               5

D[0]  =  0
D[1]  = 6^2
D[2]  = min {
        i = 2 => l[2][2] + D[1] = 25 + 36
        i = 1 => l[1][2] + D[0] = 4 + 0 = 4
    } = 4
D[3] = min {
        i = 3 => l[3][3] + D[2] = 36 + 4 = 40
        i = 2 => l[2][3] + D[1] = 4 + 36 = 40
        i = 1 => l[1][3] + D[0] = ∞ + 0 = ∞
    } = 40
D[4] = min {
        i = 4 => l[4][4] + D[3] = 16 + 40 = 56
        i = 3 => l[3][4] + D[2] = 1 + 4 = 5
        i = 2 => l[2][4] + D[1] = ∞ + 36 = ∞
        i = 1 => l[1][4] + D[0] = ∞ + 0 = ∞
    }
"""
